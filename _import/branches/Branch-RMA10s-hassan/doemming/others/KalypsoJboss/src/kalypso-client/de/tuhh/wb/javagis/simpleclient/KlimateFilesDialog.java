package de.tuhh.wb.javagis.simpleclient;
import java.awt.GridLayout;
import java.util.Date;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JTextField;

public class SimulationTimeDialog extends JPanel
{
    Vector stationNames;
    Vector fileNames;
    Vector showInResult;
    JTextField startDate;
    JTextField forcastDate;
    JTextField endDate;
    JTextField startOffset;
    JTextField endOffset;
    JButton resetTime;
    public SimulationTimeDialog()
    {
	super();
	setLayout(new GridLayout(3,3));
	startDate=new JTextField("0");
	forcastDate=new JTextField("0");
	endDate=new JTextField("0");
	startOffset=new JTextField("0");
	endOffset=new JTextField("0");
	resetTime=new JButton("reset Time");
	add(new JLabel("start of simulation"));
	add(startDate);
	add(startOffset);
	add(new JLabel("begin of forcast"));
	add(forcastDate);
	add(resetTime);
	add(new JLabel("end of simulation"));
	add(endDate);
	add(endOffset);
	doLayout();
    }
    
    public Date getStartDate() throws Exception
    {
	return fieldToDate(startDate);
    }
    public Date getForcastDate() throws Exception
    {
	return fieldToDate(forcastDate);
    }
    public Date getEndDate() throws Exception
    {
	return fieldToDate(endDate);
    }
    private Date fieldToDate(JTextField field)
    {
	return new Date();
    }
}
