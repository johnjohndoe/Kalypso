package de.tuhh.wb.javagis.simpleclient;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.GridLayout;
import java.util.Date;
import java.util.Calendar;
import java.util.GregorianCalendar;
import de.tuhh.wb.javagis.tools.I18n;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JTextField;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import de.tuhh.wb.javagis.xml.GisTransferObject;

public class SimulationTimeDialog extends JPanel implements ActionListener
{
    public final String DATE_FORMAT="d.M.yyyy H";
    public final String OFFSET_FORMAT="D H";
    JTextField startDate;
    JTextField forecastDate;
    JTextField endDate;
    JTextField startOffset;
    JTextField endOffset;
    JButton resetTime;
    public SimulationTimeDialog()
    {
	super();
	setLayout(new GridLayout(3,3));
	startDate=new JTextField(DATE_FORMAT);
	startDate.setToolTipText("Format: "+DATE_FORMAT);
	forecastDate=new JTextField(DATE_FORMAT);
	forecastDate.setToolTipText("Format: "+DATE_FORMAT);
	endDate=new JTextField(DATE_FORMAT);
	endDate.setToolTipText("Format: "+DATE_FORMAT);
	startOffset=new JTextField("1 0");
	startOffset.setToolTipText("<html>set time before forecast<br>Format: "+OFFSET_FORMAT+"</html>");
	endOffset=new JTextField("2 0");
 	endOffset.setToolTipText("<html>set time of forecast<br>Format: "+OFFSET_FORMAT+"</html>");
	resetTime=new JButton(I18n.get("KF_updateTime"));

	startDate.addActionListener(this);
	startDate.setActionCommand("date");
	forecastDate.addActionListener(this);
	forecastDate.setActionCommand("date");
	endDate.addActionListener(this);
	endDate.setActionCommand("date");
	startOffset.addActionListener(this);
	startOffset.setActionCommand("offset");
	endOffset.addActionListener(this);
	endOffset.setActionCommand("offset");
	resetTime.addActionListener(this);
	resetTime.setActionCommand("button");

	add(new JLabel(I18n.get("KF_startOfSimulation")));
	add(startDate);
	add(startOffset);
	add(new JLabel(I18n.get("KF_beginOfForecast")));
	add(forecastDate);
	add(resetTime);
	add(new JLabel(I18n.get("KF_endOfSimulation")));
	add(endDate);
	add(endOffset);
	doLayout();
    }
    
    public Date getStartDate() throws Exception
    {
	return textToDate(startDate.getText(),DATE_FORMAT);
    }
    public Date getForecastDate() throws Exception
    {
	return textToDate(forecastDate.getText(),DATE_FORMAT);
    }
    public Date getEndDate() throws Exception
    {
	return textToDate(endDate.getText(),DATE_FORMAT);
    }
    private Date getStartOffset() throws Exception
    {
	return textToDate(startOffset.getText(),OFFSET_FORMAT);
    }
    private Date getEndOffset() throws Exception
    {
	return textToDate(endOffset.getText(),OFFSET_FORMAT);
    }

    private Date textToDate(String value,String format) throws Exception
    {
	DateFormat dateFormat=new SimpleDateFormat(format);
	return dateFormat.parse((String)value);
    }
    
    private String dateToText(Date value,String format)
    {
	DateFormat dateFormat=new SimpleDateFormat(format);
	return dateFormat.format(value);
    }

    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	Object source=e.getSource();
	if("date".equals(command))
	    {
		JTextField field=((JTextField)source);
		try
		    {
			Date date=textToDate(field.getText(),DATE_FORMAT);
		    }
		catch(Exception err)
		    {
			field.setText(DATE_FORMAT);
		    }
	    }
	if("offset".equals(command))
	    {
		JTextField field=((JTextField)source);
		try
		    {
			Date date=textToDate(field.getText(),OFFSET_FORMAT);
		    }
		catch(Exception err)
		    {
			field.setText(OFFSET_FORMAT);
		    }
	    }
	if("button".equals(command))
	    {
		try
		    {
			Calendar start=new GregorianCalendar();
			Calendar end=new GregorianCalendar();
			Calendar startOffset=new GregorianCalendar();
			Calendar endOffset=new GregorianCalendar();
			Date now=new Date(System.currentTimeMillis());
			start.setTime(now);
			end.setTime(now);
			startOffset.setTime(getStartOffset());
			endOffset.setTime(getEndOffset());
			if(startOffset.get(Calendar.DAY_OF_YEAR)<365)
			    start.add(Calendar.DAY_OF_YEAR,-startOffset.get(Calendar.DAY_OF_YEAR));
			start.add(Calendar.HOUR_OF_DAY,-startOffset.get(Calendar.HOUR_OF_DAY));
			end.add(Calendar.DAY_OF_YEAR,endOffset.get(Calendar.DAY_OF_YEAR));
			end.add(Calendar.HOUR_OF_DAY,endOffset.get(Calendar.HOUR_OF_DAY));

			
			startDate.setText(dateToText(start.getTime(),DATE_FORMAT));
			endDate.setText(dateToText(end.getTime(),DATE_FORMAT));
			forecastDate.setText(dateToText(now,DATE_FORMAT));

		    }
		catch(Exception err)
		    {
			System.out.println("invalid TimeFormat :-(");
		    }
	    }
    }

    public void storeToGto(GisTransferObject gto) throws Exception
    {
	gto.addSimpleProperty("m_startDate",Long.toString(getStartDate().getTime()));
	gto.addSimpleProperty("m_forecastDate",Long.toString(getForecastDate().getTime()));
	gto.addSimpleProperty("m_endDate",Long.toString(getEndDate().getTime()));
	gto.addSimpleProperty("m_offset_start",startOffset.getText());
	gto.addSimpleProperty("m_offset_end",endOffset.getText());
    }

    public void loadFromGto(GisTransferObject gto)
    {
	try
	    {
		Date date=gto.getSimplePropertyAsDate("m_startDate");
		if(date!=null)
		    startDate.setText(dateToText(date,DATE_FORMAT));
		date=gto.getSimplePropertyAsDate("m_forecastDate");
		if(date!=null)
		    forecastDate.setText(dateToText(date,DATE_FORMAT));
		date=gto.getSimplePropertyAsDate("m_endDate");
		if(date!=null)
		    endDate.setText(dateToText(date,DATE_FORMAT));
		String text=gto.getSimpleProperty("m_offset_start");
		if(text!=null)
		    startOffset.setText(text);
		text=gto.getSimpleProperty("m_offset_end");
		if(text!=null)
		    endOffset.setText(text);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("couldn not read dates");
	    }
    }
    
}
