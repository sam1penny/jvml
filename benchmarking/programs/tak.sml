fun tak (x,y,z) =
   if not (y < x)
      then z
   else tak (tak (x - 1, y, z),
             tak (y - 1, z, x),
             tak (z - 1, x, y))

structure Main =
   struct
      fun doit n =
         if n = 0
            then ()
         else (tak (33,22,11);
                doit (n - 1))
   end
