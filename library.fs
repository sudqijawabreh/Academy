module Library
open DomainPrimitiveTypes
open System
open System.Text
open Rop
type CourseError=
|CourseName
let OptionFoldPrint f  defualt a=
    match a with
    Some x-> f x
    |None->
        defualt


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
        name:String50.T;
        description:string;
        course:String50.T;
        dependentOn:Lesson option ;
        contentUrl:UrlString.T ;
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
        name:String50.T;
        description:string;
        icon:string;
        section:String50.T;
        lessons:Lesson list

        status:CourseStatus
        }

type Section={
    name:String50.T;
    courses:Course list;
    appPath:string
    }

type Navigation=Navigation of Section list
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
let deleteCourse course=deleteItem (fun (c:Course)->c<>course)
let toSection  s c f={s with courses=s.courses|>f c}
let InSection=toSection
let addCourseToSection s c =addCourse|> toSection s c
let updateCourseInSection s c =updateCourse |>InSection s c
let deleteCourseInSection s course=deleteCourse |>InSection s course


let addLesson (lesson:Lesson) =addNewItem lesson
let updateLesson (lesson:Lesson)=updateItem(fun (l:Lesson)->if lesson.name=l.name then lesson else l) lesson
let deleteLesson lesson=deleteItem (fun (l:Lesson)->l<>lesson)
let toCourse c  l f={c with lessons=c.lessons|>f l}
let inCourse =toCourse
let addLessonToCourse c  l =addLesson |>toCourse c  l
let updateLessonInCourse c  l=updateLesson |>inCourse c  l
let deleteLessonInCourse  c l=deleteLesson |>inCourse  c l



let addQuestion (question:QuizQuestion)=addNewItem question
let updateQuestion (quizQuestion:QuizQuestion)=updateItem(fun q->if quizQuestion.question=q.question then quizQuestion else q) quizQuestion
let deleteQuestion questionString=deleteItem (fun (quizQuestion)->quizQuestion.question<>questionString)
let toQuiz quiz question f={quiz with quizQuestions=quiz.quizQuestions|> f question}
let InQuiz=toQuiz
let addQuestionToQuiz  quiz question=  addQuestion |>toQuiz quiz question
let updateQuestionInQuiz quiz question=  updateQuestion |>InQuiz quiz question
let deleteQuestionInQuiz quiz name=  deleteQuestion |>InQuiz quiz name

let printList f l=
    l
    |>List.map(f)
    |>List.iteri (fun i x-> printfn "%i-%s" i x )
let printSections =printList (fun x-> String50.value x.name)
let printCourses = printList (fun ( x:Course )-> String50.value x.name)
let printLessons =printList (fun ( x:Lesson )-> String50.value x.name)
let createSection  appPath courses name=
    {name=name;appPath=appPath;courses=courses}
let createCourse  ds icon section lessons  status name number=
    {name=name;courseNumber=number;description=ds;icon=icon;section=section.name;lessons=lessons;status=status}
let createCourseR name number ds icon section lessons  status  =
    let x=liftR ( createCourse  ds icon section lessons status  ) name
    applyR x number
let createSectionR name path courses =
    liftR (createSection path courses) name

let createLesson ds ( course:Course ) dependentOn roles status feature progress name number url=
    {name=name;
    number=number;
    description=ds;
    course=course.name;
    dependentOn=dependentOn;
    contentUrl=url;
    assginedSecurityRoles=roles;
    Lesson.publishStatus=status;
    feature=feature;
    progress=progress;
    }
let createLessonR name number ds course dependentOn url roles status feature progress =
    lift3R ( createLesson ds course dependentOn roles status feature progress ) name number url
type DomainMessage=
    |CourseNameMustNotBeMoreThan10Chars
    |SectionNameMustNotBeMoreThan10Chars
    |LessonNameMustNotBeMoreThan10Chars
    |NotValidUrl
    |CourseNumberMustBeInteger
    |LessonNumberMustBeInteger
    |ElementIndexMustBePositive
    |ElementIndexMustBeInRange
    |ElementIndexMustBeInteger

let mapErrorToMessage =function
    |CourseNameMustNotBeMoreThan10Chars->"Course Name Must Not Be More Than 10 Chars"
    |SectionNameMustNotBeMoreThan10Chars->"Section Name Must Not Be More Than 10 Chars"
    |LessonNameMustNotBeMoreThan10Chars->"Lesson Name Must Not Be More Than 10 Chars"
    |NotValidUrl->"Invalid Url"
    |CourseNumberMustBeInteger->"Course Number Must be Integer"
    |LessonNumberMustBeInteger->"Lesson Number Must be Integer"
    |ElementIndexMustBePositive->"Index Must Be Positive"
    |ElementIndexMustBeInRange->"Index Must be in range"
    |ElementIndexMustBeInteger->"index must be integer"
let errorsToMessage errors=
    errors|>List.map(mapErrorToMessage)|>List.reduce(fun acc x->acc+" "+x)
let eitherSuccessOrOrigianl original result =
    match result with
    |Success (s,_)->s,[]
    |Failure f->original,f
let nameErrorMap error name =
    let map =function
    |StringError.MustNotBeLongerThan _ ->error
    String50.create name|>mapMessagesR map

let createCourseName=nameErrorMap CourseNameMustNotBeMoreThan10Chars
let createSectioName=nameErrorMap SectionNameMustNotBeMoreThan10Chars
let createLessonName=nameErrorMap LessonNameMustNotBeMoreThan10Chars 
let createLessonUrl url=
    let map=function
    |UrlError.NotAnUrl->NotValidUrl
    UrlString.create url|>mapMessagesR map
let createNumber error number =
    let map=function
    |IntError.NotAnInteger->error
    parseInt number|>mapMessagesR map
let createCourseNumber =createNumber CourseNumberMustBeInteger
let createLessonNumber =createNumber CourseNumberMustBeInteger
let createElementIndex  length index=
    let map=function
    |ListIndexError.MustbeLessThan length->ElementIndexMustBeInRange
    |ListIndexError.MustbePositive->ElementIndexMustBePositive
    |ListIndexError.NotAnInteger->ElementIndexMustBeInteger
    ListIndex.create length index|>mapMessagesR map
let inputSection _=
    Console.Clear()
    printfn "enter name: "
    let name=Console.ReadLine()|>createSectioName
    printfn "path :"
    let appPath=Console.ReadLine()
    createSectionR name appPath []
let inputCourse section=
    Console.Clear()
    printfn "Course name:"
    let name=Console.ReadLine()|>createCourseName
    printfn "Course number:"
    let number=Console.ReadLine()|>createCourseNumber
    printfn "Course Description:"
    let ds=Console.ReadLine()
    printfn "Course Icon:"
    let icon=Console.ReadLine()
    createCourseR name number ds icon section [] CourseStatus.NotStarted
let printSection section=
    
    printfn" 1- %s"  ( String50.value section.name )
    printfn" 2- %s" section.appPath
    printfn" 3- courses"
let printOptions ()=
    printf "(a)dd\t"
    printf "(d)elete\t"
    printf "(u)pdate\t"
    printfn "(b)ack"
let printCourse course=
    printfn "1- Course number: %i" course.courseNumber
    printfn "2- Course Name : %s" (course.name|>String50.value)
    printfn "3- Course Description: %s" course.description
    printfn "4- Course icon : %s" course.icon
    printfn "5- Course section: %s" ( course.section|>String50.value )
    printfn "6- lessons"
let inputLesson ( course:Course ) =
    Console.Clear()
    printfn "Lesson name:"
    let name=Console.ReadLine()|>createLessonName
    printfn "Lesson number:"
    let number=Console.ReadLine()|>createLessonNumber
    printfn "Lesson Description:"
    let ds=Console.ReadLine()
    printfn "Lesson conentUrl:"
    let url=Console.ReadLine()|>createLessonUrl
    createLessonR name number ds course None url [] PublishStatus.UnPublished New LessonProgress.UnCompleted
let printLesson lesson=
    printfn "1- Lesson number: %i" lesson.number
    printfn "2- Lesson Name : %s" (lesson.name|>String50.value)
    printfn "3- Lesson Description: %s" lesson.description
    printfn "4- Lesson course: %s" (lesson.course|>String50.value)
    printfn "5- contetn url: %s" (UrlString.value lesson.contentUrl)


let lessonMenu lesson=
    Console.Clear()
    printLesson lesson
    lesson


let lessonsMenu course =
    Console.Clear()
    printList(fun (x:Lesson)->string50Value x.name) course.lessons
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
            inputLesson course
            |>addLessonToCourse course
        |"d"->
            Console.ReadLine()
            |>int
            |>(fun x ->course.lessons.[x])
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
    printList(fun ( x:Course )->match x.name with String50 y->y ) section.courses
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
            inputCourse section
            |>addCourseToSection section
        |"d"->
            Console.ReadLine()
            |>int
            |>(fun x ->section.courses.[x])
            |>deleteCourseInSection section
        |"b"->section

let rec sectionMenu section =
    Console.Clear()
    printSection section
    printfn "enter number or (b) to go back : "
    match Console.ReadLine() with
    "3"-> sectionMenu ( CoursesMenu section )
    |"b"->section


    //printSections(fun ( x:Course )->match x.name with String50 y->y ) section.courses
let rec navMenu ( nav:Section list )=
    Console.Clear()
    printSections nav
    printOptions()
    match Console.ReadLine() with
    "u"->
        printf "enter number: "
        Console.ReadLine()
        |>createListIndex nav.Length
        |>Option.map (fun ( ListIndex x )->sectionMenu nav.[x])
        |>OptionFoldPrint (updateSectionInNav nav) nav
        |>navMenu
    |"a"->
        printfn "u"
        inputSection()
        |>addSectionToNav nav
        |>navMenu
    |"d"->
        Console.ReadLine()
        |>int
        |>(fun x ->nav.[x])
        |>deleteSectionInNav nav
        |>navMenu
    |"b"->nav
    |_->
        printfn "Error"
        (navMenu nav)
let menu printCollection elementMenu update delete add read getCollection record =
    let rec inside  record msg =
        Console.Clear()
        printfn "%s" msg
        let ( collection:'a list )=getCollection record
        printCollection collection
        printOptions()
        match Console.ReadLine() with
        "u"->
            printf "enter number: "
            Console.ReadLine()
            |>createElementIndex collection.Length
            |>liftR ListIndex.value
            |>liftR (fun x -> elementMenu collection.[x])
            |>liftR (update record)
            |>eitherSuccessOrOrigianl record 
            |>(fun (x,y)->y|>errorsToMessage|>inside x)
        |"a"->
            record
            |>read
            |>liftR(add record)
            |>eitherSuccessOrOrigianl record
            |>(fun (x,y)->y|>errorsToMessage|>inside x)
        |"d"->
            Console.ReadLine()
            |>createElementIndex collection.Length
            |>liftR ListIndex.value
            |>liftR (fun x -> elementMenu collection.[x])
            |>liftR (update record)
            |>eitherSuccessOrOrigianl record 
            |>(fun (x,y)->y|>errorsToMessage|>inside x)
        |"b"->record
        |_->inside record "wrong input"

    inside record ""
let rec elementMenu  printElement i nextMenu  ( element:'a )=
    let choice=sprintf "%i" i
    let rec inside ( element:'a ) :'a=
        Console.Clear()
        printElement element
        printfn "enter number or (b) to go back : "
        let c= Console.ReadLine()
        if c=choice then
            inside (nextMenu element)
        else if(c="b") then
            element
        else
            inside element

    inside element

// let lsMenu=menu printLessons lessonMenu updateLessonInCourse deleteLessonInCourse addLessonToCourse inputLesson (fun x->x.lessons)
// let cMenu=elementMenu printCourse 6 lsMenu
// let csMenu =menu printCourses cMenu updateCourseInSection deleteCourseInSection addCourseToSection inputCourse (fun x->x.courses)
// let sMenu =elementMenu printSection 3 csMenu
// let vMenu=menu printSections sMenu updateSectionInNav deleteSectionInNav addSectionToNav inputSection (fun x->x)

[<EntryPoint>]
let main argv =
    printf "hello"
    0 // return an integer exit code
