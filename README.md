# Portfolio-2

My second project I am posting is my first project for COBOL. I open up listing the environment division and the configuration division. Within the input output section I select the input file “Inventory-Totals” and select the output file “Report-File”
Then I open the Data division. Within that I open up the file and read it in line by line. Inventory – Rec reads in the data from the Inventory-Totals. Then I make the report file. The report file with have
73-character lines. 
Next I open up the Working Storage Section. Within the first part of the working storage. I make variable such as EOF-FLAG Proper Spacing and Is Prod Name Same. They will come into work later in the program.
The next line I use cobols built in data division to use the data
After that I make temp variables. Those will be used to hold the data being read in as a buffer.
After that I start listing the header lines according to the printer spacing chart
The first one lists the famous "Dr.Cheeb". It lists a pic clause of alphanumeric pic clause of 8 and then the rest is fillers. The other heading – line.  It fills in the information according to the printer spacing chart. IT is the same for the other heading lines.

The next division is the Procedure division. 
100 Is the main routine division that calls on 120- House keeping, 130- Read-Files, and 300- End-Routine
120 house keeping opens the inputs and the output file. It moves the current- date function to the working storage of current date data. It then perfroms 140 Header Writer
140 header writer moves all of the data from the heading lines to the report rec and writes it.

At the same time 130 reads the files into untill the EOF flag = no and then it will stop.  as long as it is not end it will perfrom 200- process rerturn

In process 200 Process Rtn. If the prod name is the same is equal to 'New' then it will move the id into the name same variable as well as move the name in to  it will then pull 175 paragram layout.
After that 300 end routin runs and it calucaltes the grand toatl by adding the qty holder and grand qty. 