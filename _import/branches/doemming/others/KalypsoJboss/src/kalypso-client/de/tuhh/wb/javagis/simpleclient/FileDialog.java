package de.tuhh.wb.javagis.simpleclient;
import java.io.File;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;
import javax.swing.JFileChooser;
import de.tuhh.wb.javagis.xml.GisTransferObject;

public class FileDialog extends JPanel implements ActionListener
{
    File file;
    JButton button;
    JFileChooser fileChooser;    
    String myTitle;
    
    public FileDialog(String title,boolean onlyDirs)
    {
	super();
	myTitle=title;
	fileChooser=new JFileChooser();
	if(onlyDirs)
	    fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	
	file=new File("");
	button=new JButton("");
	button.addActionListener(this);
	add(button);
	updateButton();
    }

    private void updateButton()
    {
	button.setText(myTitle+": "+file.getName());
	button.setToolTipText(myTitle+": "+file.toString());	
	setSize(getPreferredSize());
	doLayout();
    }

    public File getFile() throws Exception
    {
	if(file==null)
	    throw new Exception(myTitle+" is not set");
	return file;
    }

    public void actionPerformed(ActionEvent e)
    {
	int returnVal = fileChooser.showDialog(this, "choose "+myTitle);
	if(returnVal == JFileChooser.APPROVE_OPTION)
	    {
		file=fileChooser.getSelectedFile();
		updateButton();
	    }		
    }

    public void loadFromGto(GisTransferObject gto)
    {
	String fileName=gto.getSimpleProperty("file_"+myTitle);
	String dirName=gto.getSimpleProperty("template_dir_"+myTitle);
	if(fileName!=null)
	    file=new File(fileName);
	if(dirName!=null)
	    fileChooser.setCurrentDirectory(new File(dirName));
	updateButton();
    }
    
    public void storeToGto(GisTransferObject gto) throws Exception
    {
	gto.addSimpleProperty("file_"+myTitle,file.toString());
	gto.addSimpleProperty("template_dir_"+myTitle,fileChooser.getCurrentDirectory().toString());
    }
}
