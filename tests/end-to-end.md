issues:

1. the template class csv does not parse.
2. the error should be nicer (not a crash, but a log entry and a redirect), when error thrown, it should be caught by the runAction
3. create user page is not implemented (github issue?)
4. acid-state snapshot thread is probably missing.  needed for staging!
5. delete of user is not implemented (github issue?)
6. quorums in admin: make reasonable lower/upper limits; make fields wide enough to contain the entire number.  (especially % values over 100 :-) make sure unparseable numbers are also reported as errors ('-aaa')
7. User settings page should redirect to itself.
8. Student can't like wild idea on idea view page.




------------------------------------------------------------------------------------

test scripts  2016-04-07 andorp, fisx

- Create Initial Database (e.g. with http://localhost:8080/api/manage-state/create-init)
   - includes: admin user, school space
- Login as admin
- Change admin user's settings (ISSUE 7)
- Create user moderator1
- Create user principal1
- Create a class (112a) (ISSUE 1,2)
- Create a class (112b)
- Add student1 to class 112a (ISSUE 3)
- Select student1 in class 112a and move it to class 112b (in "Nutzer bearbeiten")
- Select student1 in class 112b and change its role to guest (in "Nutzer bearbeiten")
- Delete the student1 (ISSUE 5) (in "Nutzer bearbeiten")
- search for user in search form (or is it filter form?) (N/A)
- Edit class: Change name from (112a) to (112c)
- Edit class: Change name from (112c) to (112a)
- Edit class: Change name from (112a) to (112c)
- Select student2 in class 112a and move it to class 112b (in "Klasse bearbeiten")
- Select student2 in class 112b and change its role to guest (in "Klasse bearbeiten")
- Delete the student2 (ISSUE 5) (in "Klasse bearbeiten")
- Set Quorum for School (ISSUE 6)
- Set Quorum for "Klasse" (ISSUE 6)
- Set Phase duration for "Ausarbeitungsphase" (elaboration) (ISSUE 6)
- Set Phase duration for "Abstimmung" (voting) (ISSUE 6)
- Logout admin
- Open "/user/settings" path

- Login as student3
- Change the user settings.
- Create an idea1 at school level
- Comment comment1 on idea1
- Comment comment2 on its comment1 on idea1
- Comment comment3 on its comment2 on idea1
- Vote on comment1
- Vote on comment2
- Vote on comment3
- Like idea1 (ISSUE 8)
- Report the idea1 ("melden") (N/A)
- FIlter wild ideas by category.
- Upload avatar on own user profile (N/A)
- View ideas on own user profile
- View inbound delegations on own user profile (N/A except for html dummy)
- View outbound delegations on own user profile (N/A)
- Edit own User profile (N/A)
- Create idea2 in class of student3
- Comment comment4 on idea2
- Comment comment5 on its comment4 on idea2
- Comment comment6 on its comment5 on idea2
- Vote on comment4
- Vote on comment5
- Vote on comment6
- Create idea3 in class of student3
- Create idea4 in class of student3
- Delegate vote (select on idea, select a user on the view idea page)
- Edit idea1
- Move idea1 (N/A)
- Delete idea1
- Logout

- Login as moderator1
- Change User settings
- Create topic1 from idea2
- Edit topic1: change title, description
- Remove idea1 from topic1 (N/A)
- Add idea1, idea3 to topic1
- Logout

- Trigger timeout for topic1 (e.g. curl http://localhost:8080/testing/topic/[TID]/timeout)

* Marking ideas
- Login as principal1
- mark idea1 as feasible in topic1
- mark idea3 as not feasible in topic1
- logout

* Voting on ideas
* Mark winning ideas


TODO: Download user logs from protocol page
TODO: Reset password (twice, via "Klasse" and "Nutzer")
TODO: Delegation


QUESTION:
- When we can delete an idea?
- In which phase an idea can be removed from a topic?
