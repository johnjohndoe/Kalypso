package de.tuhh.wb.javagis.view.trafoview;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileWriter;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.tools.xml.ServiceTools;
import de.tuhh.wb.javagis.view.ViewManager;

public class TrafoView extends JInternalFrame implements InternalFrameListener,ActionListener
{
    private static File inputFile=null;
    private static File trafoFile=null;
    private static JFileChooser inputFileChooser=new JFileChooser();
    private static JFileChooser trafoFileChooser=new JFileChooser(new File("xsl"));
    private static JFileChooser outputFileChooser=new JFileChooser();

    private JButton selectInputFile=new JButton(I18n.get("TrafoView.selectInputFile"));
    private JButton selectTrafoFile=new JButton(I18n.get("TrafoView.selectTrafoFile"));
    private JButton startTrafo=new JButton(I18n.get("TrafoView.startTrafo"));
    private JPanel buttonPanel=new JPanel();
    private JPanel infoPanel=new JPanel();
    private JLabel infoInput=new JLabel();
    private JLabel infoTrafo=new JLabel();
    private static TrafoView instance=null;

    public static void openTrafoView()
    {
	if(instance==null)
	    {	    
		instance=new TrafoView(I18n.get("TrafoView.WindowTitle"));

	    }
	ViewManager.addToDesktop(instance);
	instance.show();
	//	instance.moveToFront();
    }

    private TrafoView(String title)
    {
	super(title,true,true,true,true);
	
	selectInputFile.setActionCommand("selectInput");
	selectInputFile.addActionListener(this);

	selectTrafoFile.setActionCommand("selectTrafo");
	selectTrafoFile.addActionListener(this);


	startTrafo.setActionCommand("startTrafo");
	startTrafo.addActionListener(this);

	getContentPane().setLayout(new BorderLayout());

	buttonPanel.setLayout(new BorderLayout());
	buttonPanel.add(selectInputFile,BorderLayout.NORTH);
	buttonPanel.add(selectTrafoFile,BorderLayout.SOUTH);
	
	infoPanel.setLayout(new BorderLayout());
	infoPanel.add(infoInput,BorderLayout.NORTH);
	infoPanel.add(infoTrafo,BorderLayout.SOUTH);


	getContentPane().add(buttonPanel,BorderLayout.NORTH);
	getContentPane().add(infoPanel,BorderLayout.CENTER);
	getContentPane().add(startTrafo,BorderLayout.SOUTH);


	updateStatus();
	updateInfoPanel();
	pack();
	this.addInternalFrameListener(this);
	this.moveToFront();
    }

    public void updateStatus()
    {
	if(inputFile!=null && trafoFile!=null && inputFile.exists() && trafoFile.exists())
	    {
		startTrafo.setEnabled(true);
	    }
	else
	    {
		startTrafo.setEnabled(false);
	    }
    }
    
    public void actionPerformed(ActionEvent e)
    {
	String action=e.getActionCommand();
	if("selectInput".equals(action))
	    {
		int returnVal = inputFileChooser.showDialog(this,I18n.get("TrafoView.selectInputFile"));
		if(returnVal == JFileChooser.APPROVE_OPTION)
		    {
			inputFile=inputFileChooser.getSelectedFile();
		    }
	    }
	if("selectTrafo".equals(action))
	    {
		int returnVal = trafoFileChooser.showDialog(this,I18n.get("TrafoView.selectTrafoFile"));
		if(returnVal == JFileChooser.APPROVE_OPTION)
		    {
			trafoFile=trafoFileChooser.getSelectedFile();
		    }
	    }
	if("startTrafo".equals(action))
	    {
		try
		    {
			
			Runtime.getRuntime().gc();
			String result=ServiceTools.xslTransform(inputFile,trafoFile);
			
			int returnVal = outputFileChooser.showDialog(this,I18n.get("TrafoView.selectOutputFile"));
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				File outputFile=outputFileChooser.getSelectedFile();
				FileWriter out=new FileWriter(outputFile);
				out.write(result);
				out.close();
			    }
			result=null;
		    }
		catch(Exception err)
		    {
			JOptionPane.showMessageDialog(this,I18n.get("TrafoView.ErrorMessage")+" \""+err.getMessage()+"\"",I18n.get("TrafoView.ErrorTitle"),JOptionPane.INFORMATION_MESSAGE);
			err.printStackTrace();
		    }
		finally
		    {
			Runtime.getRuntime().gc();
		    }
	    }
	updateStatus();
	updateInfoPanel();
    }
    
    public void updateInfoPanel()
    {
	if(inputFile!=null)
	    infoInput.setText(inputFile.getPath());
	else
	    infoInput.setText(I18n.get("TrafoView.notSelected"));
	if(trafoFile!=null)
	    infoTrafo.setText(trafoFile.getPath());
	else
	    infoTrafo.setText(I18n.get("TrafoView.notSelected"));
	pack();
	repaint();
    }

    // internalFrameListener:

    //          Invoked when an internal frame is activated.
    public void internalFrameActivated(InternalFrameEvent e)
    {}

    //          Invoked when an internal frame has been closed.
    public void internalFrameClosed(InternalFrameEvent e)
    {
    }

    //          Invoked when an internal frame is in the process of being closed.
    public void internalFrameClosing(InternalFrameEvent e)
    {
    }

    //          Invoked when an internal frame is de-activated.
    public void internalFrameDeactivated(InternalFrameEvent e)
    {}

    //          Invoked when an internal frame is de-iconified.
    public void internalFrameDeiconified(InternalFrameEvent e)
    {}
    
    //          Invoked when an internal frame is iconified.
    public void internalFrameIconified(InternalFrameEvent e)
    {}
    
    public void internalFrameOpened(InternalFrameEvent e)
    {}
}
