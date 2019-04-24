module Keys exposing (keyNameToCode)


keyNameToCode name =
    case name of
        "cancel" ->
            Just 3

        "help" ->
            Just 6

        "backSpace" ->
            Just 8

        "tab" ->
            Just 9

        "clear" ->
            Just 12

        "return" ->
            Just 13

        "enter" ->
            Just 14

        "shift" ->
            Just 16

        "control" ->
            Just 17

        "alt" ->
            Just 18

        "pause" ->
            Just 19

        "capsLock" ->
            Just 20

        "escape" ->
            Just 27

        "space" ->
            Just 32

        "pageUp" ->
            Just 33

        "pageDown" ->
            Just 34

        "end" ->
            Just 35

        "home" ->
            Just 36

        "left" ->
            Just 37

        "up" ->
            Just 38

        "right" ->
            Just 39

        "down" ->
            Just 40

        "printscreen" ->
            Just 44

        "insert" ->
            Just 45

        "delete" ->
            Just 46

        "0" ->
            Just 48

        "1" ->
            Just 49

        "2" ->
            Just 50

        "3" ->
            Just 51

        "4" ->
            Just 52

        "5" ->
            Just 53

        "6" ->
            Just 54

        "7" ->
            Just 55

        "8" ->
            Just 56

        "9" ->
            Just 57

        "semicolon" ->
            Just 59

        "equals" ->
            Just 61

        "a" ->
            Just 65

        "b" ->
            Just 66

        "c" ->
            Just 67

        "d" ->
            Just 68

        "e" ->
            Just 69

        "f" ->
            Just 70

        "g" ->
            Just 71

        "h" ->
            Just 72

        "i" ->
            Just 73

        "j" ->
            Just 74

        "k" ->
            Just 75

        "l" ->
            Just 76

        "m" ->
            Just 77

        "n" ->
            Just 78

        "o" ->
            Just 79

        "p" ->
            Just 80

        "q" ->
            Just 81

        "r" ->
            Just 82

        "s" ->
            Just 83

        "t" ->
            Just 84

        "u" ->
            Just 85

        "v" ->
            Just 86

        "w" ->
            Just 87

        "x" ->
            Just 88

        "y" ->
            Just 89

        "z" ->
            Just 90

        "leftCmd" ->
            Just 91

        "rightCmd" ->
            Just 93

        "contextMenu" ->
            Just 93

        "numpad0" ->
            Just 96

        "numpad1" ->
            Just 97

        "numpad2" ->
            Just 98

        "numpad3" ->
            Just 99

        "numpad4" ->
            Just 100

        "numpad5" ->
            Just 101

        "numpad6" ->
            Just 102

        "numpad7" ->
            Just 103

        "numpad8" ->
            Just 104

        "numpad9" ->
            Just 105

        "multiply" ->
            Just 106

        "add" ->
            Just 107

        "separator" ->
            Just 108

        "subtract" ->
            Just 109

        "decimal" ->
            Just 110

        "divide" ->
            Just 111

        "f1" ->
            Just 112

        "f2" ->
            Just 113

        "f3" ->
            Just 114

        "f4" ->
            Just 115

        "f5" ->
            Just 116

        "f6" ->
            Just 117

        "f7" ->
            Just 118

        "f8" ->
            Just 119

        "f9" ->
            Just 120

        "f10" ->
            Just 121

        "f11" ->
            Just 122

        "f12" ->
            Just 123

        "f13" ->
            Just 124

        "f14" ->
            Just 125

        "f15" ->
            Just 126

        "f16" ->
            Just 127

        "f17" ->
            Just 128

        "f18" ->
            Just 129

        "f19" ->
            Just 130

        "f20" ->
            Just 131

        "f21" ->
            Just 132

        "f22" ->
            Just 133

        "f23" ->
            Just 134

        "f24" ->
            Just 135

        "numLock" ->
            Just 144

        "scrollLock" ->
            Just 145

        "comma" ->
            Just 188

        "period" ->
            Just 190

        "slash" ->
            Just 191

        "backQuote" ->
            Just 192

        "openBracket" ->
            Just 219

        "backSlash" ->
            Just 220

        "closeBracket" ->
            Just 221

        "quote" ->
            Just 222

        "meta" ->
            Just 224

        _ ->
            Nothing
