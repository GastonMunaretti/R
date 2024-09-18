Use DW


Select  A.IdUsuario, E.Genero, E.Ciudad, E.Convenio, DATEDIFF(Day, E.FechaNacimiento, GetDate()) / 365 Edad,
           Case When( round( (Sum(Case When Ausentismo Is Not Null And B.Licencia <> 'llegada tarde/salida antic.' Then 1.00 Else 0.00 End) / Count(A.IdUsuario)) * 100,0)) > 15 
Then 1 Else 0 End As 'Faltador'

From   F_GESTIONDELTIEMPO A
             Join D_LICENCIAS B on
                    A.IdLicencia = B.IdLicencia
             Join D_PUESTOS C on
                    A.IdPuesto = C.idPuesto
             Join D_UNIDADORGANIGRAMA D on
                    A.IdUnidadOrganigrama = D.idUnidadOrganigrama 
              Join D_USUARIOS E On
                    A.IdUsuario = E.idUsuario


Where  A.EsFranco = 0 And        
              A.EsVacacion = 0 And
             A.IdGerencia <> 901 And
             A.Bloqueado = 1 And
             B.Licencia Not In ('COMISION','COMPENSATORIO') And
             A.Citacion Not In ('990','991','992','993','994','995','996') And
             C.Puesto <> 'GERENTE' And
             D.UnidadOrganigrama Not In ('Delegado','Licencias Especiales') 
              

Group bY A.IdUsuario, E.Genero, E.Ciudad, E.Convenio, DATEDIFF(Day, E.FechaNacimiento, GetDate()) / 365

Order By 1  
