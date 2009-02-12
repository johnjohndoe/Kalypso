/**
 * JHouranMinuteChooser.java
 *
 * @author Created by Omnicore CodeGuide
 */
package de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar;

import java.awt.GridLayout;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;

import de.tuhh.wb.javagis.tools.I18n;


public class JHourandMinuteChooser extends JPanel
{
	Calendar calendar = Calendar.getInstance();
	SpinnerDateModel hourDateModel;
	SpinnerDateModel minuteDateModel;
	
	public JHourandMinuteChooser(){
		
		setLayout(new GridLayout(2,4));
		setHourChooser();
		setMinuteChooser();
			
	}
	
	public void setHourChooser(){
		/*JSpinField hourSpinner = new JSpinField();
		hourSpinner.setMinimum(0);
		hourSpinner.setMaximum(23);
		hourSpinner.setValue(calendar.get(Calendar.HOUR));*/
		hourDateModel = new SpinnerDateModel(calendar.getTime(),null,null,Calendar.HOUR_OF_DAY);
		JSpinner hourSelector = new JSpinner(hourDateModel);
		JSpinner.DateEditor hourDateEditor = new JSpinner.DateEditor(hourSelector, "HH");
		hourSelector.setEditor(hourDateEditor);
		//hourDateEditor.getTextField().setEnabled(false);
		JLabel hourLabel = new JLabel(I18n.get("JHourandMInuteChooser_Hour"));
		add(hourLabel);
		add(hourSelector);
	}
	
	public void setMinuteChooser(){
		/*JSpinField minuteSpinner = new JSpinField();
		minuteSpinner.setMinimum(00);
		minuteSpinner.setMaximum(59);
		 minuteSpinner.setValue(calendar.get(Calendar.MINUTE));*/
		minuteDateModel = new SpinnerDateModel();
		JSpinner minuteSelector = new JSpinner(minuteDateModel);
		JSpinner.DateEditor minuteDateEditor = new JSpinner.DateEditor(minuteSelector, "mm");
		minuteSelector.setEditor(minuteDateEditor);
		//minuteDateEditor.getTextField().setEnabled(false);
		JLabel minuteLabel = new JLabel(I18n.get("JHourandMInuteChooser_Minute"));
		add(minuteLabel);
		add(minuteSelector);
	}
	
	public void setHour(Date date){
	hourDateModel.setValue(date);
	}
	
	public void setMinutes(Date date){
	minuteDateModel.setValue(date);
	}
	
}

