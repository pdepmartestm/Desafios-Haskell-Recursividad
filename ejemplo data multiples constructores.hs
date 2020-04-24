data Persona = UnaPersona {
    nombre:: String,
    hijos::[ Persona]
}  | Nadie  | Alguien {dni::Int}

cuantosHijos:: Persona -> Int
cuantosHijos (UnaPersona _ hs)  = length hs
cuantosHijos (Nadie)  = 0 
cuantosHijos (Alguien nro) = nro `div` 10000000

denominacion::Persona -> String
denominacion (UnaPersona nom [])  = nom ++ " sin hijos"
denominacion (UnaPersona nom (h:hs))  = nom ++ " padre de (" ++ denominacion h ++ ") y " ++ show (length hs) ++ " hijos más"
denominacion (Nadie)  = "nn" 
denominacion (Alguien nro) = show nro

-- para que se vea por consola de una manera legible
instance Show Persona where
    show persona  = denominacion persona 


--ejemplos de prueba

juan = UnaPersona "Juan Perez" [desconocido, pedro, maria]
marta = UnaPersona "Marta Gonzalez" [juan]

desconocido = Nadie
pedro = Alguien 25111111
maria = Alguien 40000000

