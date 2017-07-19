module r365Library
open System
open System.Text
type String50= String50 of string
type ListIndex= ListIndex of int
let string50Value s=match s with String50 x ->x
let createListIndex length index=
    printfn "%i" index
    if index<length then
        ListIndex ( index )|>Some
    else
        None
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
        course:String50;
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
        section:String50;
        lessons:Lesson list

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
let deleteSection section=deleteItem (fun s->s<>section)
let addSection (section:Section)=addNewItem section
let addSectionToNav nav s=addSection s nav
let updateSectionInNav nav s=updateSection s nav
let deleteSectionInNav nav s=deleteSection s nav

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

let printSections f sections=
    sections
    |>List.map(f)
    |>List.iteri (fun i x-> printfn "%i-%s" i x )
let another =printSections (fun x->match x.name with String50 y->y)
let inputSection ()=
    Console.Clear()
    printfn "enter name: "
    let name=Console.ReadLine()
    printfn "path :"
    let appPath=Console.ReadLine()
    {name=String50 name;appPath=appPath;courses=[]}
let inputCourse section=
    Console.Clear()
    printfn "Course name:"
    let name=Console.ReadLine()
    printfn "Course number:"
    let number=Console.ReadLine()|>int
    printfn "Course Description:"
    let ds=Console.ReadLine()
    printfn "Course Icon:"
    let icon=Console.ReadLine()
    {name=(String50 name);courseNumber=number;description=ds;icon=icon;section=section;lessons=[];status=CourseStatus.NotStarted}
let printSection section=
    let (String50 name)=section.name
    printfn" 1- %s"  name
    printfn" 2- %s" section.appPath
    printfn" 3- courses"
let printOptions ()=
    printf "(a)dd\t"
    printf "(d)elete\t"
    printf "(u)pdate\t"
    printfn "(b)ack"
let printCourse course=
    printfn "1- Course number: %i" course.courseNumber
    printfn "2- Course Name : %s" (course.name|>string50Value)
    printfn "3- Course Description: %s" course.description
    printfn "4- Course icon : %s" course.icon
    printfn "5- Course section: %s" ( course.section|>string50Value )
    printfn "6- lessons"
let inputLesson course =
    Console.Clear()
    printfn "Lesson name:"
    let name=Console.ReadLine()
    printfn "Lesson number:"
    let number=Console.ReadLine()|>int
    printfn "Lesson Description:"
    let ds=Console.ReadLine()
    printfn "Lesson conentUrl:"
    let url=Console.ReadLine()
    {name=(String50 name);
    number=number;
    description=ds;
    course=course;
    dependentOn=None;
    contentUrl=url;
    assginedSecurityRoles=[];
    publishStatus=UnPublished;
    feature=Old;
    progress=UnCompleted;
    }
let printLesson lesson=
    printfn "1- Lesson number: %i" lesson.number
    printfn "2- Lesson Name : %s" (lesson.name|>string50Value)
    printfn "3- Lesson Description: %s" lesson.description
    printfn "4- Lesson course: %s" (lesson.course|>string50Value)
    printfn "5- contetn url: %s" (lesson.contentUrl)


let lessonMenu lesson=
    Console.Clear()
    printLesson lesson
    lesson


let lessonsMenu course =
    Console.Clear()
    printSections(fun (x:Lesson)->string50Value x.name) course.lessons
    printOptions ()
    match Console.ReadLine() with 
        "u"->
            printf "enter number: "
            Console.ReadLine()
            |>int
            |>(fun x->course.lessons.[x])
            |>lessonMenu 
            |>updateLessonInCourse course
            
        |"a"->
            inputLesson course.name
            |>addLessonToCourse course 
        |"d"->
            Console.ReadLine() 
            |>int 
            |>(fun x ->course.lessons.[x].name)
            |>deleteLessonInCourse course
        |"b"->course
let courseMenu course=
    Console.Clear()
    printCourse course
    match Console.ReadLine() with 
    "6"-> lessonsMenu course
    |"b"->course

let CoursesMenu section=
    Console.Clear()
    printSections(fun ( x:Course )->match x.name with String50 y->y ) section.courses
    printOptions ()
    match Console.ReadLine() with 
        "u"->
            printf "enter number: "
            Console.ReadLine()
            |>int
            |>(fun x->section.courses.[x])
            |>courseMenu 
            |>updateCourseInSection section
            
        |"a"->
            inputCourse section.name
            |>addCourseToSection section 
        |"d"->
            Console.ReadLine() 
            |>int 
            |>(fun x ->section.courses.[x].name)
            |>deleteCourseInSection section
        |"b"->section

let rec sectionMenu section =
    Console.Clear()
    printSection section
    printfn "enter number or (b) to go back : "
    let ( String50 sectionName)=section.name
    match Console.ReadLine() with 
    "3"-> sectionMenu ( CoursesMenu section )
    |"b"->section
    
    
    //printSections(fun ( x:Course )->match x.name with String50 y->y ) section.courses
let rec navMenu (Some nav)=
    Console.Clear()
    another nav
    printOptions()
    match Console.ReadLine() with 
    "u"->
        printf "enter number: "
        Console.ReadLine()
        |>int
        |>createListIndex nav.Length
        |>Option.map (fun ( ListIndex x )->sectionMenu nav.[x])
        |>Option.map (updateSectionInNav nav)
        |>navMenu
    |"a"->
        printfn "u"
        inputSection()
        |>addSectionToNav nav
        |>Some
        |>navMenu
    |"d"->
        Console.ReadLine() 
        |>int 
        |>(fun x ->nav.[x])
        |>deleteSectionInNav nav
        |>Some
        |>navMenu
    |"b"->nav
let menu (collection:'a list) printCollection elementMenu update delete add read=
    let rec inside (Some (collection:'a list))=
        Console.Clear()
        printCollection collection
        printOptions()
        match Console.ReadLine() with 
        "u"->
            printf "enter number: "
            Console.ReadLine()
            |>int
            |>createListIndex collection.Length
            |>Option.map(fun (ListIndex x )->elementMenu collection.[x])
            |>Option.map(update collection)
            |>inside
        |"a"->
            printfn "u"
            read()
            |>add collection
            |>Some
            |>inside
        |"d"->
            Console.ReadLine() 
            |>int 
            |>(fun x ->collection.[x])
            |>delete collection
            |>Some
            |>inside
        |"b"->collection
    inside (Some collection)
let rec elementMenu element printElement nextMenu i=
    let choice=sprintf "%i" i
    let rec inside e= 
        Console.Clear()
        printElement e
        printfn "enter number or (b) to go back : "
        match Console.ReadLine() with 
        choice-> inside ( nextMenu element )
        |"b"->e
    inside element
    
[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code

