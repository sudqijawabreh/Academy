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
        answers:Answers;
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
        quizQuestions:QuizQuestion list
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
let addNewItem item list =
    item::list
let updateItem f item list=
    list|>List.map(f)
let deleteItem f attrib list  =
    list|>List.filter(f)

let updateSection section=updateItem (fun s->if section.name=s.name then section else s) section
let deleteSection name=deleteItem (fun s->s.name<>name)
let addSection (section:Section)=addNewItem section

let addCourse (course:Course)=addNewItem course
let updateCourse ( course:Course )=updateItem(fun c->if course.name=c.name then course else c) course
let deleteCourse name=deleteItem (fun (c:Course)->c.name<>name)

let addLesson (lesson:Lesson)=addNewItem lesson
let updateLesson (lesson:Lesson)=updateItem(fun l->if lesson.name=l.name then lesson else l) lesson
let deleteLesson name=deleteItem (fun (l:Lesson)->l.name<>name)


let addQuestion (question:QuizQuestion)=addNewItem question
let updateQuestion (quizQuestion:QuizQuestion)=updateItem(fun q->if quizQuestion.question=q.question then quizeQuestion else q) quizeQuestion
let deleteQuestion questionString=deleteItem (fun (quizQuestion)->quizQuestion.question<>questionString)

[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code
