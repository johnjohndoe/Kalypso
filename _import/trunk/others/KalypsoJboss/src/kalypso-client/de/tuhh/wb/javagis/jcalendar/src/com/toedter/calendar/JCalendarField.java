/**
 * JCalendarField.java
 *
 * @author Created by Omnicore CodeGuide
 */

package de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Locale;

import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;

import de.tuhh.wb.javagis.jcalendar.src.com.toedter.components.JLocaleChooser;

public class JCalendarField extends JPanel implements PropertyChangeListener
{
	private JPanel calendarPanel;
	private JPanel demoPanel;
	private JCalendar jcalendar3;
	private JTextField dateField;
	private JLocaleChooser localeChooser;
	private Calendar calendar;
	//private KunststoffLookAndFeel kunststoffLnF;
	
	public JCalendarField (JCalendar calendar){
		init(calendar);
	}
	
	public void init(JCalendar calendar) {
		// Set the Kunststoff Look And Feel:
		//initializeLookAndFeel();

		this.setLayout(new BorderLayout());
		calendarPanel = new JPanel();
		calendarPanel.setLayout(new BorderLayout());

		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BorderLayout());
		/*JLocaleChooser localeChooser = new JLocaleChooser();
		localeChooser.addPropertyChangeListener(this);
		controlPanel.add(new JLabel(" Locale:   "), BorderLayout.WEST);
		 controlPanel.add(localeChooser, BorderLayout.CENTER);*/

		dateField = new JTextField();
		dateField.setEditable(false);
		controlPanel.add(dateField, BorderLayout.CENTER);

		//jcalendar3 = new JCalendar(JMonthChooser.NO_SPINNER);
		jcalendar3=calendar;
		jcalendar3.setBorder(new EmptyBorder(10, 10, 10, 10));
		jcalendar3.addPropertyChangeListener(this);
		JPanel calender = new JPanel();
		calender.add(jcalendar3);

		calendarPanel.add(controlPanel, BorderLayout.NORTH);
		calendarPanel.add(calender, BorderLayout.CENTER);

		this.add(calendarPanel, BorderLayout.CENTER);

		//this.calendar = Calendar.getInstance();
		//calendar.set(initialDate.getYear(),initialDate.getMonth(),initialDate.getDate());
		//jcalendar3.setCalendar(this.calendar);
	}
	
	public void propertyChange(PropertyChangeEvent evt) {
		if (calendarPanel != null) {
			if (evt.getPropertyName().equals("locale")) {
				jcalendar3.setLocale((Locale) evt.getNewValue());
				DateFormat df = DateFormat.getDateInstance(DateFormat.LONG,
						jcalendar3.getLocale());
				dateField.setText(df.format(calendar.getTime()));
			} else if (evt.getPropertyName().equals("calendar")) {
				calendar = (Calendar) evt.getNewValue();
				DateFormat df = DateFormat.getDateInstance(DateFormat.LONG,
						jcalendar3.getLocale());
				dateField.setText(df.format(calendar.getTime()));
				jcalendar3.setCalendar(calendar);
			}
		}
	}
		
	
	
}

