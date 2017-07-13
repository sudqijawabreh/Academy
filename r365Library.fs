module r365Library
open System
open System.Text.RegularExpressions
type String50=String50 of string
type UrlString=UrlString of string
type SecurityRole=
    ResturantManager
    |Schedular
    |ReadOnlyExecutive

type Answers={
    answer1:string;
    answer2:string;
    answer3:string option;
    answer4:string option;
    answer5:string option;
    answer6:string option;
    }
type CorrectAnswer= Answer1 |Answer2 |Answer3 |Answer4 |Answer5 |Answer6

type PublishStatus=Published|UnPublished
type LessonFeature=New|Old
type LessonProgress=Completed|UnCompleted

type CourseStatus=
    Completed of  DateTime
    |NotStarted
    |Started

type QuizStatus=
    Pass
    |Fail
type QuizQuestion={
        question:string;
        correctAnswer:CorrectAnswer;
        answers:Answers
        status:QuizStatus
        }
type Lesson={
        number:int;
        name:String50;
        description:string;
        course:string;
        dependentOn:Lesson ;
        contentUrl:string ;
        assginedSecurityRoles:SecurityRole list
        publishStatus:PublishStatus
        feature:LessonFeature
        progress:LessonProgress
        }

type Quiz={
        name:string;
        quizeQuestions:QuizQuestion list
        }

type Course={
        courseNumber:int;
        name:String50;
        description:string;
        icon:string;
        section:string;
        Lessones:Lesson list
        quiz :Quiz
        status:CourseStatus
        }

type Section={
    name:String50;
    Courses:Course list;
    appPath:string
    }

type Navigation=Navigation of Section list
let createString50 ( s:string )=
    if s.Length<=50 then
        Some (String50 s)
    else
        None
let createUrlString (s:string)=
   if  Regex.IsMatch(s,"/((([A-Za-z]{3,9}:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)") then 
    Some (UrlString s)
   else
    None
let addNewSection navigation (section:Section)=
    section::navigation
let updateSection navigation ( updatedSection:Section ) name =
    navigation|>List.map(fun s->if s.name=name then updatedSection else s)
let deleteSection navigation name=
    navigation|>List.filter(fun s->  s.name<>name)


[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code
