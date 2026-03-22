open System
open ClassSchedule.Domain

let unwrap result =
    match result with
    | Ok value -> value
    | Error err -> failwith $"Domain setup failed: {err}"

let mkTime startHour startMinute endHour endMinute =
    TimeRange.create (TimeOnly(startHour, startMinute)) (TimeOnly(endHour, endMinute))
    |> unwrap

let building = BuildingCode.create "ENG" |> unwrap
let roomNumber = RoomNumber.create "101" |> unwrap
let projector = EquipmentTag.create "Projector" |> unwrap
let whiteboard = EquipmentTag.create "WhiteBoard" |> unwrap

let teacherName = PersonName.create "Ada" (Some "M.") "Lovelace" |> unwrap
let degree = DegreeProfile.create Masters (Some "Computer Science") |> unwrap

let weeklyMorning =
    { Day = DayOfWeek.Monday
      Time = mkTime 9 0 12 0 }

let teacher =
    { Id = TeacherId(Guid.NewGuid())
      Name = teacherName
      Degree = degree
      Availability =
        { Availability.empty with
            WeeklyOpen = [ weeklyMorning ] } }

let classroom =
    Classroom.create
        (ClassroomId(Guid.NewGuid()))
        building
        roomNumber
        (Some "North Wing Lecture Room")
        24
        (set [ projector; whiteboard ])
        { Availability.empty with
            WeeklyOpen = [ weeklyMorning ] }
    |> unwrap

let course =
    Course.create (CourseId(Guid.NewGuid())) "CS-201" "Functional Programming" (Some 20) (set [ projector ])
    |> unwrap

let date = DateOnly(2026, 3, 23)
let slot = mkTime 10 0 11 0

let assignment =
    { SessionId = SessionId(Guid.NewGuid())
      CourseId = course.Id
      TeacherId = teacher.Id
      ClassroomId = classroom.Id
      Date = date
      Time = slot }

let conflicting =
    { SessionId = SessionId(Guid.NewGuid())
      CourseId = course.Id
      TeacherId = teacher.Id
      ClassroomId = classroom.Id
      Date = date
      Time = mkTime 10 30 11 30 }

let printResult label result =
    match result with
    | Ok _ -> printfn $"{label}: Assignment accepted"
    | Error conflicts -> printfn $"{label}: Rejected with %A{conflicts}"

let firstResult =
    Scheduling.validateAssignment teacher classroom course assignment []

let secondResult =
    Scheduling.validateAssignment teacher classroom course conflicting [ assignment ]

printResult "First attempt" firstResult
printResult "Second attempt" secondResult
