//! Contract for testing precompile interactions, `selfdestruct`ing to a precompile address... etc.

use alloy::sol;

sol! {
    #[sol(rpc, bytecode="")]
    contract PrecompileTarget {
        event CodeSizeChecked(address indexed addr, uint256 size);
        event CodeHashChecked(address indexed addr, bytes32 codeHash);
        event CallResult(address indexed addr, bool success);
        event BalanceChecked(address indexed addr, uint256 balance);
        event SelfDestructed(address indexed recipient);

        enum TestType {
            OnlyExtChecks,                          // 0: Only EXT* operations
            ExtChecksWithCallNoValue,               // 1: EXT* + CALL with 0 value
            ExtChecksWithCallWithValue,             // 2: EXT* + CALL with value
            ExtChecksWithStaticCall,                // 3: EXT* + STATICCALL
            ExtChecksWithDelegateCall,              // 4: EXT* + DELEGATECALL
            ExtChecksWithCallCode,                  // 5: EXT* + CALLCODE
            NestedDelegateCallNoValue,              // 6: Contract -> DelegateForwarder -> Precompile (no value)
            NestedDelegateCallWithValue,            // 7: Contract -> DelegateForwarder -> Precompile (with value)
            SelfDestructToPrecompile,               // 8: SELFDESTRUCT to precompile + EXT* checks after
            Nothing                                 // 9: Do nothing
        }

        address public delegateForwarder;

        constructor(TestType testType, address precompile) payable {
            // Deploy delegate forwarder for nested tests
            delegateForwarder = address(new DelegateForwarder());

            // Run the selected test type in constructor
            if (testType == TestType.OnlyExtChecks) {
                testOnlyExtChecks();
            } else if (testType == TestType.ExtChecksWithCallNoValue) {
                testExtChecksWithCallNoValue();
            } else if (testType == TestType.ExtChecksWithCallWithValue) {
                testExtChecksWithCallWithValue();
            } else if (testType == TestType.ExtChecksWithStaticCall) {
                testExtChecksWithStaticCall();
            } else if (testType == TestType.ExtChecksWithDelegateCall) {
                testExtChecksWithDelegateCall();
            } else if (testType == TestType.ExtChecksWithCallCode) {
                testExtChecksWithCallCode();
            } else if (testType == TestType.NestedDelegateCallNoValue) {
                testNestedDelegateCallNoValue();
            } else if (testType == TestType.NestedDelegateCallWithValue) {
                testNestedDelegateCallWithValue();
            } else if (testType == TestType.SelfDestructToPrecompile) {
                testSelfDestructToPrecompile(payable(precompile));
            } else if (testType == TestType.Nothing) {
                // Do nothing
            }
        }

        // Test 1: Only EXT* operations
        function testOnlyExtChecks() public {
            checkAllPrecompileAddresses();
        }

        // Test 2: EXT* checks + CALL with no value
        function testExtChecksWithCallNoValue() public {
            checkAllPrecompileAddressesWithCall(0);
        }

        // Test 3: EXT* checks + CALL with value
        function testExtChecksWithCallWithValue() public payable {
            uint256 valuePerCall = address(this).balance / 20; // Divide among ~20 addresses
            checkAllPrecompileAddressesWithCall(valuePerCall);
        }

        // Test 4: EXT* checks + STATICCALL
        function testExtChecksWithStaticCall() public {
            checkAllPrecompileAddressesWithStaticCall();
        }

        // Test 5: EXT* checks + DELEGATECALL
        function testExtChecksWithDelegateCall() public {
            checkAllPrecompileAddressesWithDelegateCall();
        }

        // Test 6: EXT* checks + CALLCODE
        function testExtChecksWithCallCode() public payable {
            uint256 valuePerCall = address(this).balance / 20;
            checkAllPrecompileAddressesWithCallCode(valuePerCall);
        }

        // Test 7: Nested delegatecall (no value)
        function testNestedDelegateCallNoValue() public {
            doNestedDelegateCallToAllPrecompiles();
        }

        // Test 8: Nested delegatecall (with value)
        function testNestedDelegateCallWithValue() public payable {
            doNestedDelegateCallToAllPrecompiles();
        }

        // Test 9: SELFDESTRUCT to precompile address
        function testSelfDestructToPrecompile(address payable precompileAddr) public {
            // Check EXT* operations before selfdestruct
            checkAddress(precompileAddr);

            selfdestruct(precompileAddr);

            // Check EXT* operations after selfdestruct
            checkAddress(precompileAddr);
            emit SelfDestructed(precompileAddr);
        }

        // Helper: Check a single address with all EXT* operations
        function checkAddress(address target) public {
            // EXTCODESIZE
            uint256 size;
            assembly {
                size := extcodesize(target)
            }
            emit CodeSizeChecked(target, size);

            // EXTCODECOPY
            bytes memory code = new bytes(size);
            assembly {
                extcodecopy(target, add(code, 0x20), 0, size)
            }

            // EXTCODEHASH
            bytes32 codeHash;
            assembly {
                codeHash := extcodehash(target)
            }
            emit CodeHashChecked(target, codeHash);

            // BALANCE
            uint256 bal = target.balance;
            emit BalanceChecked(target, bal);
        }

        // Helper: Check all precompile addresses (only EXT* operations)
        function checkAllPrecompileAddresses() internal {
            // Check for 0 address
            checkAddress(address(0));

            // Check for normal precompiles (0x01 to 0x11)
            for (uint160 i = 1; i <= 0x11; i++) {
                checkAddress(address(i));
            }

            // Check for p256 (0x100)
            checkAddress(address(0x100));

            // Check for random addresses next to and before p256
            checkAddress(address(0xff));   // Before 0x100
            checkAddress(address(0x101));  // After 0x100

            // Check addresses around the last normal precompile
            checkAddress(address(0x12));   // After 0x11
        }

        // Helper: Check all precompile addresses with CALL
        function checkAllPrecompileAddressesWithCall(uint256 valuePerCall) internal {
            address[19] memory targets = [
                address(0),
                address(0x01), address(0x02), address(0x03), address(0x04),
                address(0x05), address(0x06), address(0x07), address(0x08),
                address(0x09), address(0x0a), address(0x0b), address(0x0c),
                address(0x0d), address(0x0e), address(0x0f), address(0x10),
                address(0x11), address(0x100)
            ];

            for (uint256 i = 0; i < targets.length; i++) {
                address target = targets[i];

                // Check EXT* operations
                checkAddress(target);

                // Check balance before
                uint256 balBefore = target.balance;
                emit BalanceChecked(target, balBefore);

                // CALL
                (bool success,) = target.call{value: valuePerCall}("");
                emit CallResult(target, success);

                // Check balance after
                uint256 balAfter = target.balance;
                emit BalanceChecked(target, balAfter);

                // Check EXT* operations
                checkAddress(target);
            }
        }

        // Helper: Check all precompile addresses with STATICCALL
        function checkAllPrecompileAddressesWithStaticCall() internal {
            address[19] memory targets = [
                address(0),
                address(0x01), address(0x02), address(0x03), address(0x04),
                address(0x05), address(0x06), address(0x07), address(0x08),
                address(0x09), address(0x0a), address(0x0b), address(0x0c),
                address(0x0d), address(0x0e), address(0x0f), address(0x10),
                address(0x11), address(0x100)
            ];

            for (uint256 i = 0; i < targets.length; i++) {
                address target = targets[i];

                // Check EXT* operations
                checkAddress(target);

                // STATICCALL
                (bool success,) = target.staticcall("");

                // Check EXT* operations
                checkAddress(target);
            }
        }

        // Helper: Check all precompile addresses with DELEGATECALL
        function checkAllPrecompileAddressesWithDelegateCall() internal {
            address[19] memory targets = [
                address(0),
                address(0x01), address(0x02), address(0x03), address(0x04),
                address(0x05), address(0x06), address(0x07), address(0x08),
                address(0x09), address(0x0a), address(0x0b), address(0x0c),
                address(0x0d), address(0x0e), address(0x0f), address(0x10),
                address(0x11), address(0x100)
            ];

            for (uint256 i = 0; i < targets.length; i++) {
                address target = targets[i];

                // Check EXT* operations
                checkAddress(target);

                // Check balance before
                uint256 balBefore = target.balance;
                emit BalanceChecked(target, balBefore);

                // DELEGATECALL
                (bool success,) = target.delegatecall("");
                emit CallResult(target, success);

                // Check balance after
                uint256 balAfter = target.balance;
                emit BalanceChecked(target, balAfter);

                // Check EXT* operations
                checkAddress(target);
            }
        }

        // Helper: Check all precompile addresses with CALLCODE
        function checkAllPrecompileAddressesWithCallCode(uint256 valuePerCall) internal {
            address[19] memory targets = [
                address(0),
                address(0x01), address(0x02), address(0x03), address(0x04),
                address(0x05), address(0x06), address(0x07), address(0x08),
                address(0x09), address(0x0a), address(0x0b), address(0x0c),
                address(0x0d), address(0x0e), address(0x0f), address(0x10),
                address(0x11), address(0x100)
            ];

            for (uint256 i = 0; i < targets.length; i++) {
                address target = targets[i];

                // Check EXT* operations
                checkAddress(target);

                // Check balance before
                uint256 balBefore = target.balance;
                emit BalanceChecked(target, balBefore);

                // CALLCODE (deprecated but still works)
                bool success;
                assembly {
                    success := callcode(gas(), target, valuePerCall, 0, 0, 0, 0)
                }
                emit CallResult(target, success);

                // Check balance after
                uint256 balAfter = target.balance;
                emit BalanceChecked(target, balAfter);

                // Check EXT* operations
                checkAddress(target);
            }
        }

        // Helper: Nested delegatecall through DelegateForwarder
        function doNestedDelegateCallToAllPrecompiles() internal {
            address[19] memory targets = [
                address(0),
                address(0x01), address(0x02), address(0x03), address(0x04),
                address(0x05), address(0x06), address(0x07), address(0x08),
                address(0x09), address(0x0a), address(0x0b), address(0x0c),
                address(0x0d), address(0x0e), address(0x0f), address(0x10),
                address(0x11), address(0x100)
            ];

            for (uint256 i = 0; i < targets.length; i++) {
                address target = targets[i];

                // Check EXT* operations
                checkAddress(target);

                // Check balance before
                uint256 balBefore = target.balance;
                emit BalanceChecked(target, balBefore);

                // Encode target address + any data for the nested delegatecall
                bytes memory data = abi.encodePacked(target);

                // Delegatecall to forwarder which will delegatecall to precompile
                (bool success,) = delegateForwarder.delegatecall(data);
                emit CallResult(target, success);

                // Check balance after
                uint256 balAfter = target.balance;
                emit BalanceChecked(target, balAfter);

                // Check EXT* operations
                checkAddress(target);
            }
        }

        // Allow contract to receive ether
        receive() external payable {}
    }
}
