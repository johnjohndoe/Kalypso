package de.tuhh.wb.javagis.simpleclient;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import de.tuhh.wb.javagis.xml.GisTransferObject;

public class TimeStepDialog extends JPanel
{
    JLabel text;
    JTextField field;

    public TimeStepDialog()
    {
	super();
	text=new JLabel("TimeStep: [h]");
	field=new JTextField("1",8);
	field.setToolTipText("<html>You must consider that the simulation<br> supports not more than <b>1000 steps</b>,<br>when choosing times and steps</html>");
	add(text);
	add(field);
	doLayout();
    }
    
    public Integer getTimeStep() throws Exception
    {
	return new Integer(field.getText());
    }
    
    public void storeToGto(GisTransferObject gto)
    {
	gto.addSimpleProperty("m_timeStep",field.getText());
    }
    public void loadFromGto(GisTransferObject gto)
    {
	String text=gto.getSimpleProperty("m_timeStep");
	if(text!=null)
	    field.setText(text);
    }
}
