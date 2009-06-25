package de.tuhh.wb.javagis.simpleclient;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
//import java.awt.GridLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Date;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JScrollPane;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JTextField;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.VectorSet;
import org.xml.sax.helpers.AttributesImpl;

public class StationFileDialog extends JScrollPane implements ActionListener//,MouseListener
{
    public final static String RAIN_KEY="rainseq";
    public final static String TEMP_KEY="tempseq";

    JPanel panel;
    String myTitle;
    String myKey;
    Vector stationNames;
    Vector fileNames;
    Vector ignoreTemplate;
    Vector showInResults;
    
    JButton addStation;
    JFileChooser fileChooser;
    public StationFileDialog(String key,String title)
    {
	super();
	this.panel=new JPanel(new GridBagLayout());
	this.myTitle=title;
	this.myKey=key;
	fileChooser=new JFileChooser();
	stationNames=new Vector();
	fileNames=new Vector();
	ignoreTemplate=new Vector();

	showInResults=new Vector();
	addStation=new JButton("add Station");
	addStation.setActionCommand("addStation");
	addStation.addActionListener(this);
	update();
    }
    
    private void update()
    {
	//	removeAll();
	panel.removeAll();
	//	panel.setLayout(new GridLayout(stationNames.size()+2,4));
	JLabel jStation=new JLabel("station (filename)");
	jStation.setToolTipText("<html>filename must correspond to the filenames"
				+"<br>in the <b>catchments table</b> <u>and</u> in"
				+"<br>in the <b>stations table</b>."
				+"<br><b>filenames are always <u>case sensitivity</u></b>.</html>");
	add2View(jStation);
	add2View(new JLabel("forecast-sequence"));

	JLabel ignoreLabel= new JLabel("use template ");
	ignoreLabel.setToolTipText("<html>selected means:"+
				   "<li>before forecast time use DB-series"+
				   "<br>after that use file-series</li>"+
				   "unselected means:"+
				   "<li>use DB-series all the time</html");
	add2ViewLastInRow(ignoreLabel);
	//	add2ViewLastInRow(new JLabel("visualize "));

	for(int i=0;i<stationNames.size();i++)
	    {
		JTextField field=(JTextField)stationNames.elementAt(i);
		field.setActionCommand("removeStation"+String.valueOf(i));
		add2View(field);
		JButton button=new JButton(((File)fileNames.elementAt(i)).getName());
		button.setToolTipText(fileNames.elementAt(i).toString());
		button.setActionCommand("setFile"+String.valueOf(i));
		button.addActionListener(this);
		add2View(button);
		add2ViewLastInRow((JComponent)ignoreTemplate.elementAt(i));
		//		add2ViewLastInRow((JComponent)showInResults.elementAt(i));
	    }
	add2ViewLastInRow(addStation);
	//	panel.setPreferredSize(new Dimension(300,300));
	setViewportView(panel);
	panel.setVisible(true);
	setPreferredSize(new Dimension(400,200));
	doLayout();
	repaint(); setVisible(true);
    }   

    public synchronized void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	System.out.println(command);
	if("addStation".equals(command))
	    {
		addStation();
		update();
	    }
	if(command.startsWith("setFile"))
	    {
		try
		    {
			int row=Integer.parseInt(command.substring(7));
			// chose FileName:
			int returnVal = fileChooser.showDialog(this, "choose "+myTitle+" sequence");
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				fileNames.set(row,fileChooser.getSelectedFile());
				update();
			    }		
		    }
		catch(Exception err)
		    {
			err.printStackTrace();
		    }
	    }

	if(command.startsWith("removeStation"))
	    {
		int row=Integer.parseInt(command.substring(13));
		// chose FileName:
		JTextField text=(JTextField) stationNames.elementAt(row);
		if(text.getText().equals(""))
		    removeStation(row);
	    }
    }

    public void addStation()
    {
	JTextField field=new JTextField("new station");
	stationNames.add(field);
	field.addActionListener(this);
	fileNames.add(new File(fileChooser.getCurrentDirectory(),"default.txt"));
	showInResults.add(new JCheckBox());
	ignoreTemplate.add(new JCheckBox());
    }

    public void addStation(String name,File file,boolean show,boolean ignore)
    {
	JTextField field=new JTextField(name);
	stationNames.add(field);
	field.addActionListener(this);
	fileNames.add(file);
	JCheckBox showBox=new JCheckBox();
	JCheckBox ignoreBox=new JCheckBox();
	showBox.setSelected(show);
	showInResults.add(showBox);
	ignoreBox.setSelected(ignore);
	ignoreTemplate.add(ignoreBox);
    }

    public  void removeStation(int row)
    {
	stationNames.removeElementAt(row);
	fileNames.removeElementAt(row);
	showInResults.removeElementAt(row);
	ignoreTemplate.removeElementAt(row);
	System.out.println("remove Station");
	update();
    }

    public void add2View(JComponent  component) 
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = 1;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layoutConstraints.anchor = GridBagConstraints.CENTER;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }

    public void add2ViewLastInRow(JComponent  component) 
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.anchor = GridBagConstraints.CENTER;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }

    public void storeToGto(GisTransferObject gto)
    {
	VectorSet vs=new VectorSet(myKey);
	
	String name=null;
	File file=null;
	Boolean show=null;
	Boolean ignore=null;
	
	for(int i=0;i<stationNames.size();i++)
	    {
		AttributesImpl atts=new AttributesImpl();
		name=((JTextField)stationNames.elementAt(i)).getText();
		file=(File)fileNames.elementAt(i);
		show=new Boolean(((JCheckBox)showInResults.elementAt(i)).isSelected());
		ignore=new Boolean(!((JCheckBox)ignoreTemplate.elementAt(i)).isSelected());
		atts.addAttribute("","name","","xsi:string",name);
		atts.addAttribute("","file","","xsi:string",file.toString());
		atts.addAttribute("","show","","xsi:string",show.toString());
		atts.addAttribute("","ignore","","xsi:string",ignore.toString());
		vs.addRow(atts);
	    }
	gto.addVectorSet(vs);
	gto.addSimpleProperty("template_dir_"+myKey,fileChooser.getCurrentDirectory().toString());
    }
    
    public void loadFromGto(GisTransferObject gto)
    {
	String name=null;
	String file=null;
	boolean show=false;
	boolean ignore=false;
	
	VectorSet vs=gto.getVectorSet(myKey);
	for(int i=0;i<vs.size();i++)
	    {
		name=vs.getSimpleProperty("name",i);
		file=vs.getSimpleProperty("file",i);
		show=vs.getSimplePropertyAsBoolean("show",i);
		ignore=!vs.getSimplePropertyAsBoolean("ignore",i);
		if(name!=null && file !=null)
		    addStation(name,new File(file),show,ignore);
	    }
	String dirName=gto.getSimpleProperty("template_dir_"+myKey);
	if(dirName!=null)
	    fileChooser.setCurrentDirectory(new File(dirName));
	update();
    }
}    
