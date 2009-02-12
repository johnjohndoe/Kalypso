package de.tuhh.wb.javagis.view;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import de.tuhh.wb.javagis.data.GisElement;

public class ValueEditor  implements ChangeListener, DocumentListener, FocusListener
{
    private GisElement myGisElement;
    private int myPosition; // position of this SimpleProperrty;
    private JComponent myRepresentation;
    private boolean notSet;

    public ValueEditor(GisElement gisElement, int valuePosition)
    {
	this.myGisElement=gisElement;
	this.myPosition=valuePosition;
	this.notSet=true;

	Class valueClass=myGisElement.getSimplePropertyClass(myPosition);
	Object value=myGisElement.getSimplePropertyValue(myPosition);
	if(valueClass==java.lang.Boolean.class)
	    {
		boolean selected=((java.lang.Boolean)value).booleanValue();
		JCheckBox checkBox=new JCheckBox("",selected);
		checkBox.setEnabled(false);
		checkBox.addChangeListener(this);
		myRepresentation=(JComponent)checkBox;		
	    }
	else
	    {
		String text;
		if(value!=null)
		    {
			text=value.toString();
			notSet=false;
		    }
		else 
		    {
			text="";
			notSet=true;
		    }
		JLabel textField=new JLabel(text);		
		//		JTextField textField=new JTextField(text,8);		
		//		textField.getDocument().addDocumentListener(this);
		//		textField.addFocusListener(this);
		myRepresentation=(JComponent)textField;		
	    }
    }
    
    public JComponent getRepresentation()
    {
	return myRepresentation;
    }

    public void stateChanged(ChangeEvent e)
    {
	System.out.println("stateChanged.");
    }

    // DocumentListener:

    //           Gives notification that an attribute or set of attributes changed.
    public void changedUpdate(DocumentEvent e) 
    {
	System.out.println("Document.changeUpdate");
    }
    // Gives notification that there was an insert into the document
    public void insertUpdate(DocumentEvent e) 
    {
	System.out.println("Document.insertUpdate");
    } 
    // Gives notification that a portion of the document has been removed.
    public void removeUpdate(DocumentEvent e) 
    {
	System.out.println("Document.removeUpdate");
    }

    // FocusListener
    public void focusGained(FocusEvent e) 
    {
	System.out.println("Focus won...");
    }

    public void focusLost(FocusEvent e) 
    {
	System.out.println("Focus lost...");
 	Class valueClass=myGisElement.getSimplePropertyClass(myPosition);
	Object value=myGisElement.getSimplePropertyValue(myPosition);
	/*	if(valueClass==java.lang.Boolean.class)	
	    {}
	else
	    {
		String text=(JTextField)myRepresentation.getText();
		if(valueClass==java.lang.Integer)
		    value=new Integer(text);
				this.notSet=false;
	    }
	*/
    }
}
