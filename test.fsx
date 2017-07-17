#load "r365Library.fs"
open r365Library
let answers= {Answers.answer1="1";answer2="2";answer3=None;answer4=None;answer5=None;answer6=None}
let q1={question="hello";correctAnswer=Answer1;answers=answers}
let quiz={name="quiz";Quiz.quizQuestions=[];status=Untaken}
let list=addQuestion q1 quiz.quizQuestions
let quiz1={quiz with quizQuestions=list}
let x=Option.get((createString50 "hello"))
let coursename= Option.get((createString50 "course name"))
let lessonname= Option.get((createString50 "lesson name"))

let s={name=x;Courses=[];appPath=""}
let c1={
        courseNumber=1;
        name=String50 "course 1";
        description="description";
        icon="icon";
        section="section";
        Lessones=[];
        quiz=quiz1;
        status=NotStarted

        }
let l1={
        number=1;
        name=String50 "lesson1 "
        description="description"
        course="course 1"
        dependentOn=None;
        contentUrl= "http://www.foo.com";
        assginedSecurityRoles=[ReadOnlyExecutive];
        publishStatus=Published;
        feature=New;
        progress=UnCompleted;
        }
let l2={
        number=2;
        name=String50 "lesson 2"
        description="another description"
        course="course 1"
        dependentOn=Some l1;
        contentUrl= "http://www.foo.com";
        assginedSecurityRoles=[];
        publishStatus=Published;
        feature=Old;
        progress=LessonProgress.Completed;
}
let c2={
        courseNumber=1;
        name=coursename;
        description="description";
        icon="icon";
        section="section";
        Lessones=[];
        quiz=quiz1;
        status=NotStarted
        }