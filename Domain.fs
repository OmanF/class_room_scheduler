module ClassSchedule.Domain

open System

// Validation failures for domain value objects.
type ValidationError =
    | EmptyField of string
    | InvalidField of string

// Reasons why a scheduling attempt failed.
type SchedulingConflict =
    | TeacherNotAvailable
    | ClassroomNotAvailable
    | TeacherAlreadyScheduled
    | ClassroomAlreadyScheduled
    | ClassroomCapacityTooSmall of required: int * actual: int
    | MissingRequiredEquipment of missing: string list

[<Struct>]
type ClassroomId = ClassroomId of Guid

[<Struct>]
type TeacherId = TeacherId of Guid

[<Struct>]
type CourseId = CourseId of Guid

[<Struct>]
type SessionId = SessionId of Guid

type PersonName =
    private
        { First: string
          Middle: string option
          Last: string }

module PersonName =
    let create (first: string) (middle: string option) (last: string) =
        let normalizeOptional (value: string option) =
            value
            |> Option.map (fun x -> x.Trim())
            |> Option.bind (fun x -> if String.IsNullOrWhiteSpace x then None else Some x)

        let firstTrimmed = first.Trim()
        let lastTrimmed = last.Trim()

        if String.IsNullOrWhiteSpace firstTrimmed then
            Error(EmptyField "Teacher first name")
        elif String.IsNullOrWhiteSpace lastTrimmed then
            Error(EmptyField "Teacher last name")
        else
            Ok
                { First = firstTrimmed
                  Middle = normalizeOptional middle
                  Last = lastTrimmed }

    let toDisplayName (name: PersonName) =
        match name.Middle with
        | Some middle -> $$$"""%%%s{{{name.First}}} %%%s{{{middle}}} %%%s{{{name.Last}}}"""
        | None -> $$$"""%%%s{{{name.First}}} %%%s{{{name.Last}}}"""

type DegreeTitle =
    | Bachelors
    | Masters
    | Doctorate
    | Professor
    | Other of string

type DegreeProfile =
    private
        { Title: DegreeTitle
          Specialty: string option }

module DegreeProfile =
    let create (title: DegreeTitle) (specialty: string option) =
        let specialtyTrimmed = specialty |> Option.map (fun s -> s.Trim())


        match title with
        | Other custom when String.IsNullOrWhiteSpace(custom.Trim()) -> Error(InvalidField "Degree title")
        | _ when specialtyTrimmed |> Option.exists String.IsNullOrWhiteSpace -> Error(EmptyField "Degree specialty")
        | _ ->
            Ok
                { Title = title
                  Specialty = specialtyTrimmed }

type RoomNumber = private RoomNumber of string

module RoomNumber =
    let create (value: string) =
        let trimmed = value.Trim()

        if String.IsNullOrWhiteSpace trimmed then
            Error(EmptyField "Room number")
        else
            Ok(RoomNumber trimmed)

    let value (RoomNumber number) = number

type BuildingCode = private BuildingCode of string

module BuildingCode =
    let create (value: string) =
        let trimmed = value.Trim()

        if String.IsNullOrWhiteSpace trimmed then
            Error(EmptyField "Building code")
        else
            Ok(BuildingCode trimmed)

    let value (BuildingCode code) = code

type EquipmentTag = private EquipmentTag of string

module EquipmentTag =
    let create (value: string) =
        let trimmed = value.Trim().ToLowerInvariant()

        if String.IsNullOrWhiteSpace trimmed then
            Error(EmptyField "Equipment tag")
        else
            Ok(EquipmentTag trimmed)

    let value (EquipmentTag tag) = tag

type TimeRange =
    private
        { StartTime: TimeOnly
          EndTime: TimeOnly }

module TimeRange =
    let create (startTime: TimeOnly) (endTime: TimeOnly) =
        if startTime >= endTime then
            Error(InvalidField "Time range")
        else
            Ok
                { StartTime = startTime
                  EndTime = endTime }

    let overlaps (left: TimeRange) (right: TimeRange) =
        left.StartTime < right.EndTime && right.StartTime < left.EndTime

    let contains (outer: TimeRange) (inner: TimeRange) =
        outer.StartTime <= inner.StartTime && outer.EndTime >= inner.EndTime

type WeeklyAvailability = { Day: DayOfWeek; Time: TimeRange }

type DateAvailability = { Date: DateOnly; Time: TimeRange }

type Availability =
    { WeeklyOpen: WeeklyAvailability list
      WeeklyBlocked: WeeklyAvailability list
      DateOpen: DateAvailability list
      DateBlocked: DateAvailability list }

module Availability =
    let empty =
        { WeeklyOpen = []
          WeeklyBlocked = []
          DateOpen = []
          DateBlocked = [] }

    let isOpenOnDate (date: DateOnly) (time: TimeRange) (availability: Availability) =
        let weeklyOpen =
            availability.WeeklyOpen
            |> List.exists (fun x -> x.Day = date.DayOfWeek && TimeRange.contains x.Time time)

        let dateOpen =
            availability.DateOpen
            |> List.exists (fun x -> x.Date = date && TimeRange.contains x.Time time)

        let weeklyBlocked =
            availability.WeeklyBlocked
            |> List.exists (fun x -> x.Day = date.DayOfWeek && TimeRange.overlaps x.Time time)

        let dateBlocked =
            availability.DateBlocked
            |> List.exists (fun x -> x.Date = date && TimeRange.overlaps x.Time time)

        (weeklyOpen || dateOpen) && not (weeklyBlocked || dateBlocked)

type Classroom =
    { Id: ClassroomId
      Building: BuildingCode
      RoomNumber: RoomNumber
      Name: string option
      Capacity: int
      Equipment: Set<EquipmentTag>
      Availability: Availability }

module Classroom =
    let create
        (id: ClassroomId)
        (building: BuildingCode)
        (roomNumber: RoomNumber)
        (name: string option)
        (capacity: int)
        (equipment: Set<EquipmentTag>)
        (availability: Availability)
        =
        let normalizedName =
            name
            |> Option.map (fun x -> x.Trim())
            |> Option.bind (fun x -> if String.IsNullOrWhiteSpace x then None else Some x)

        if capacity <= 0 then
            Error(InvalidField "Classroom capacity")
        else
            Ok
                { Id = id
                  Building = building
                  RoomNumber = roomNumber
                  Name = normalizedName
                  Capacity = capacity
                  Equipment = equipment
                  Availability = availability }

type Teacher =
    { Id: TeacherId
      Name: PersonName
      Degree: DegreeProfile
      Availability: Availability }

type Course =
    { Id: CourseId
      Code: string
      Title: string
      MinimumCapacity: int option
      RequiredEquipment: Set<EquipmentTag> }

module Course =
    let create
        (id: CourseId)
        (code: string)
        (title: string)
        (minimumCapacity: int option)
        (requiredEquipment: Set<EquipmentTag>)
        =
        let codeTrimmed = code.Trim()
        let titleTrimmed = title.Trim()

        if String.IsNullOrWhiteSpace codeTrimmed then
            Error(EmptyField "Course code")
        elif String.IsNullOrWhiteSpace titleTrimmed then
            Error(EmptyField "Course title")
        else
            match minimumCapacity with
            | Some value when value <= 0 -> Error(InvalidField "Course minimum capacity")
            | _ ->
                Ok
                    { Id = id
                      Code = codeTrimmed
                      Title = titleTrimmed
                      MinimumCapacity = minimumCapacity
                      RequiredEquipment = requiredEquipment }

type Session =
    { Id: SessionId
      CourseId: CourseId
      TeacherId: TeacherId }

type Assignment =
    { SessionId: SessionId
      CourseId: CourseId
      TeacherId: TeacherId
      ClassroomId: ClassroomId
      Date: DateOnly
      Time: TimeRange }

module Scheduling =
    let private missingEquipment (required: Set<EquipmentTag>) (available: Set<EquipmentTag>) =
        Set.difference required available |> Set.toList |> List.map EquipmentTag.value

    let validateAssignment
        (teacher: Teacher)
        (classroom: Classroom)
        (course: Course)
        (candidate: Assignment)
        (existing: Assignment list)
        =
        let conflicts = ResizeArray<SchedulingConflict>()

        if not (Availability.isOpenOnDate candidate.Date candidate.Time teacher.Availability) then
            conflicts.Add TeacherNotAvailable

        if not (Availability.isOpenOnDate candidate.Date candidate.Time classroom.Availability) then
            conflicts.Add ClassroomNotAvailable

        let teacherBusy =
            existing
            |> List.exists (fun x ->
                x.TeacherId = candidate.TeacherId
                && x.Date = candidate.Date
                && TimeRange.overlaps x.Time candidate.Time)

        if teacherBusy then
            conflicts.Add TeacherAlreadyScheduled

        let roomBusy =
            existing
            |> List.exists (fun x ->
                x.ClassroomId = candidate.ClassroomId
                && x.Date = candidate.Date
                && TimeRange.overlaps x.Time candidate.Time)

        if roomBusy then
            conflicts.Add ClassroomAlreadyScheduled

        match course.MinimumCapacity with
        | Some required when classroom.Capacity < required ->
            conflicts.Add(ClassroomCapacityTooSmall(required, classroom.Capacity))
        | _ -> ()

        let missing = missingEquipment course.RequiredEquipment classroom.Equipment

        if missing.Length > 0 then
            conflicts.Add(MissingRequiredEquipment missing)

        if conflicts.Count = 0 then
            Ok candidate
        else
            Error(List.ofSeq conflicts)
