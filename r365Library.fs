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
        name:string;
        descrtiptoin:string;
        icon:string;
        section:string;
        Lessones:Lesson list
        status:CourseStatus
        }

type Section={
    name:string;
    Courses:Course list;
    appPath:string
    }

[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code
