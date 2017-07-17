module r365Library
open System
open System.Text
type String50= String50 of string

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
    |Untaken
type QuizQuestion={
    question:string;
        correctAnswer:CorrectAnswer;
        answers:Answers;
}
        
type Lesson={
        number:int;
        name:String50;
        description:string;
        course:string;
        dependentOn:Lesson option ;
        contentUrl:string ;
        assginedSecurityRoles:SecurityRole list
        publishStatus:PublishStatus
        feature:LessonFeature
        progress:LessonProgress
        }

type Quiz={
        name:string;
        quizQuestions:QuizQuestion list
        status:QuizStatus
        }

type Course={
        courseNumber:int;
        name:String50;
        description:string;
        icon:string;
        section:string;
        lessons:Lesson list
        quiz :Quiz
        status:CourseStatus
        }

type Section={
    name:String50;
    courses:Course list;
    appPath:string
    }

type Navigation=Navigation of Section list
let createString50 ( s:string )=
    if s.Length<=50 then
        Some (String50 s)
    else
        None
let createUrlString (s:string)=
    Some (UrlString s)
   //if  Regex.IsMatch(s,"/((([A-Za-z]{3,9}:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)") then 
let addNewItem item list =
    item::list
let updateItem f item list=
    list|>List.map(f)
let deleteItem f list  =
    list|>List.filter(f)

let updateSection section=updateItem (fun s->if section.name=s.name then section else s) section
let deleteSection name=deleteItem (fun s->s.name<>name)
let addSection (section:Section)=addNewItem section
let addSectionToNav s nav=addSection nav s

let addCourse (course:Course)=addNewItem course
let updateCourse ( course:Course )=updateItem(fun (c:Course)->if course.name=c.name then course else c) course
let deleteCourse name=deleteItem (fun (c:Course)->c.name<>name)
let toSection  s c f={s with courses=s.courses|>f c}
let InSection=toSection
let addCourseToSection s c =addCourse|> toSection s c
let updateCourseInSection s c =updateCourse |>InSection s c
let deleteCourseInSection s name=deleteCourse |>InSection s name


let addLesson (lesson:Lesson) =addNewItem lesson
let updateLesson (lesson:Lesson)=updateItem(fun (l:Lesson)->if lesson.name=l.name then lesson else l) lesson
let deleteLesson name=deleteItem (fun (l:Lesson)->l.name<>name)
let toCourse c  l f={c with lessons=c.lessons|>f l}
let inCourse =toCourse
let addLessonToCourse c  l =addLesson |>toCourse c  l 
let updateLessonInCourse c  l=updateLesson |>inCourse c  l
let deleteLessonInCourse  c name=deleteLesson |>inCourse  c name



let addQuestion (question:QuizQuestion)=addNewItem question
let updateQuestion (quizQuestion:QuizQuestion)=updateItem(fun q->if quizQuestion.question=q.question then quizQuestion else q) quizQuestion
let deleteQuestion questionString=deleteItem (fun (quizQuestion)->quizQuestion.question<>questionString)
let toQuiz quiz question f={quiz with quizQuestions=quiz.quizQuestions|> f question}
let InQuiz=toQuiz
let addQuestionToQuiz  quiz question=  addQuestion |>toQuiz quiz question
let updateQuestionInQuiz quiz question=  updateQuestion |>InQuiz quiz question
let deleteQuestionInQuiz quiz name=  deleteQuestion |>InQuiz quiz name
let answers= {Answers.answer1="1";answer2="2";answer3=None;answer4=None;answer5=None;answer6=None}
let q1={question="hello";correctAnswer=Answer1;answers=answers}
let quiz={name="quiz";Quiz.quizQuestions=[];status=Untaken}

let printSections sections=
    sections
    |>List.map(fun x->match x.name with String50 y->y)
    |>List.iteri (fun i x-> printfn "%i-%s" i x )


[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code

