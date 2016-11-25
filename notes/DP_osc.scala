val t = UDP.Transmitter("192.168.0.21" -> 7771)
t.connect()
// t ! osc.Message("/yo-mann")
t ! osc.Message("/recImp", 0f, 3840f, 1f)  // DP schwarz
t ! osc.Message("/recImp", 0f, 0f, 1f)     // DP voll da

t ! osc.Message("/recImp", 3400f, 3840f, 1f)
t ! osc.Message("/recImp", 3400f, 1f + 3840, 1f) // make sure x2 > x1
t ! osc.Message("/recImp", 3000f, 3900f, 1f)
