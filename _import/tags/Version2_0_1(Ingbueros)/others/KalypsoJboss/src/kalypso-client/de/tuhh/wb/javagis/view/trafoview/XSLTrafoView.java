package de.tuhh.wb.javagis.view.trafoview;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.tools.Shape2GML;
import de.tuhh.wb.javagis.tools.xml.ServiceTools;
import de.tuhh.wb.javagis.view.WaitingDialog;

public class XSLTrafoView
	extends JFrame
	implements InternalFrameListener, ActionListener {
	private static File inputFile = null;
	//private static File trafoFile = null;
	private static File outputFile = null;
	private static JFileChooser inputFileChooser = new JFileChooser();
	//private static JFileChooser trafoFileChooser=new JFileChooser(new File("xsl"));
	private JComboBox jComboBox_Transformations;
	private static JFileChooser outputFileChooser = new JFileChooser();

	//private JButton selectInputFile=new JButton(I18n.get("TrafoView.selectInputFile"));
	//private JButton selectTrafoFile=new JButton(I18n.get("TrafoView.selectTrafoFile"));
	private JLabel label_selectInputFile =
		new JLabel(I18n.get("TrafoView.LabelInputFile"));
	private JLabel label_selectTransformation =
		new JLabel(I18n.get("TrafoView.LabelTrafoFile"));
	private JLabel label_selectOutputFile =
		new JLabel(I18n.get("TrafoView.LabelOutputFile"));
	private JButton startTrafo = new JButton(I18n.get("TrafoView.startTrafo"));
	private JPanel buttonPanel = new JPanel();
	private JPanel startPanel = new JPanel();
	//private JPanel infoPanel=new JPanel();
	//private JLabel infoInput=new JLabel();
	//private JLabel infoTrafo=new JLabel();
	private JButton button_selectInputFile = new JButton(I18n.get("TrafoView.Choose"));
	private JButton button_selectOutputFile = new JButton(I18n.get("TrafoView.Choose"));
	private static XSLTrafoView instance = null;


	private XSLTrafoView(String title) {
		super(title);

		/*selectInputFile.setActionCommand("selectInput");
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
		getContentPane().add(startTrafo,BorderLayout.SOUTH);*/

		makeView();
		setSize(500, 150);

		updateStatus();
		//updateInfoPanel();
		//pack();
				
	}

	private void makeView() {

		Vector transformations = getTransformations();
		jComboBox_Transformations = new JComboBox(transformations);
		jComboBox_Transformations.setRenderer(new MyCellRenderer());

		button_selectInputFile.setActionCommand("select InputFile");
		button_selectInputFile.addActionListener(this);
		button_selectInputFile.setBackground(Color.white);
		button_selectInputFile.setBorderPainted(false);
		button_selectInputFile.setMargin(new Insets(0, 0, 0, 0));

		button_selectOutputFile.setActionCommand("select OutputFile");
		button_selectOutputFile.addActionListener(this);
		button_selectOutputFile.setBackground(Color.white);
		button_selectOutputFile.setBorderPainted(false);
		button_selectOutputFile.setMargin(new Insets(0, 0, 0, 0));

		startTrafo.setActionCommand("startTrafo");
		startTrafo.addActionListener(this);

		buttonPanel.setLayout(new GridLayout(3, 2));
		buttonPanel.add(label_selectInputFile);
		buttonPanel.add(button_selectInputFile);
		buttonPanel.add(label_selectTransformation);
		buttonPanel.add(jComboBox_Transformations);
		buttonPanel.add(label_selectOutputFile);
		buttonPanel.add(button_selectOutputFile);

		startPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		startPanel.add(startTrafo);

		getContentPane().setLayout(new GridLayout(2, 1));
		getContentPane().add(buttonPanel);
		getContentPane().add(startPanel);
	}

	private Vector getTransformations() {
    Vector transformationFiles = new Vector();
    
    File parentFile = new File("xsl/Transformationen");
		File[] files = parentFile.listFiles();
		for (int i = 0; i < files.length; i++) {
			if (files[i].getName().startsWith("trafo_")) 
      		transformationFiles.addElement(files[i]);
		}
    parentFile = new File("xsl");
    files = parentFile.listFiles();
    for (int i = 0; i < files.length; i++) {
      if (files[i].getName().startsWith("trafo_")) 
          transformationFiles.addElement(files[i]);
    }
    
		//add Shape2GML
		transformationFiles.addElement(new String("shape_2_gml"));

		/*System.out.println(
			"Anzahl Transformationen: " + transformationFiles.size());
		for (int k = 0; k < transformationFiles.size(); k++) {
			Object trafo = transformationFiles.elementAt(k);
			System.out.println("Class: " + trafo.getClass());
			if (trafo instanceof File) {
				File trafoFile = (File) trafo;
				System.out.println("File: " + trafoFile.getPath());
			} else {
				System.out.println("String: " + trafo);
			}
		}*/

		return transformationFiles;
	}

	public void updateStatus() {
		if (inputFile != null && outputFile != null && inputFile.exists()) {
			startTrafo.setEnabled(true);
		} else {
			startTrafo.setEnabled(false);
		}
	}

	public void actionPerformed(ActionEvent e) {
		String action = e.getActionCommand();
		/*if("selectInput".equals(action))
		    {
			int returnVal = inputFileChooser.showDialog(this,I18n.get("TrafoView.selectInputFile"));
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				inputFile=inputFileChooser.getSelectedFile();
			    }
		    }*/
		/*if("selectTrafo".equals(action))
		    {
			int returnVal = trafoFileChooser.showDialog(this,I18n.get("TrafoView.selectTrafoFile"));
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				trafoFile=trafoFileChooser.getSelectedFile();
			    }
		    }*/
		if ("select InputFile".equals(action)) {
			int returnVal =
				inputFileChooser.showDialog(
					this,
					I18n.get("TrafoView.selectInputFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				inputFile = inputFileChooser.getSelectedFile();
				button_selectInputFile.setText(
					inputFileChooser.getSelectedFile().getPath());
			} else {
				button_selectInputFile.setText(I18n.get("TrafoView.Choose"));
				inputFile = null;
			}
		}

		if ("select OutputFile".equals(action)) {
			int returnVal =
				outputFileChooser.showDialog(
					this,
					I18n.get("TrafoView.selectOutputFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				outputFile = outputFileChooser.getSelectedFile();
				button_selectOutputFile.setText(
					outputFileChooser.getSelectedFile().getPath());
			} else {
				button_selectOutputFile.setText(I18n.get("TrafoView.Choose"));
				outputFile = null;
			}
		}

		if ("startTrafo".equals(action)) {
			final Component gp = getGlassPane();
		  gp.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			Thread t = new Thread(new Runnable() {
				public void run() {
					try {
						Runtime.getRuntime().gc();
						Object trafo =
							jComboBox_Transformations.getSelectedItem();
						System.out.println("Transformation: " + trafo);
						if (trafo instanceof File) {
							File trafoFile = (File) trafo;
								ServiceTools.xslTransform(inputFile, trafoFile,outputFile);
							
							/*int returnVal =
								outputFileChooser.showDialog(
									this,
									I18n.get("TrafoView.selectOutputFile"));
							if (returnVal == JFileChooser.APPROVE_OPTION) {
								File outputFile = outputFileChooser.getSelectedFile();
								FileWriter out = new FileWriter(outputFile);
								out.write(result);
								out.close();
							}*/							
						}
						if (trafo instanceof String) {
							String trafoName = (String) trafo;
							if (trafoName.equals("shape_2_gml")) {
								String newInputFile = inputFile.getPath().replaceAll("\\.shp","");
								Shape2GML shape2GML =
									new Shape2GML(
										newInputFile,
										outputFile.getPath());
							}
						}
					} catch (Exception err) {
						WaitingDialog.waitingDialogDispose();
						gp.setVisible(false);
						JOptionPane.showMessageDialog(
							instance,
							I18n.get("TrafoView.ErrorMessage")
								+ " \""
								+ err.getMessage()
								+ "\"",
							I18n.get("TrafoView.ErrorTitle"),
							JOptionPane.INFORMATION_MESSAGE);
						err.printStackTrace();
					} finally {
						Runtime.getRuntime().gc();
					}
					WaitingDialog.waitingDialogDispose();
					gp.setVisible(false);
					if(outputFile.exists()){
					JOptionPane.showMessageDialog(
						instance,
						I18n.get("TrafoView.FinishMessage")+"\n"+I18n.get("TrafoView.FinishMessage_OutputFile")+outputFile.getPath(),
						I18n.get("TrafoView.FinishMessageTitle"),
						JOptionPane.INFORMATION_MESSAGE);
					}
				}
			});
			t.start();
			WaitingDialog waitDialog = WaitingDialog.getInstance();
			gp.setVisible(true);

		}
		updateStatus();
		//updateInfoPanel();
	}

	/*public void updateInfoPanel()
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
	}*/

	// internalFrameListener:

	//          Invoked when an internal frame is activated.
	public void internalFrameActivated(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame has been closed.
	public void internalFrameClosed(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is in the process of being closed.
	public void internalFrameClosing(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is de-activated.
	public void internalFrameDeactivated(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is de-iconified.
	public void internalFrameDeiconified(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is iconified.
	public void internalFrameIconified(InternalFrameEvent e) {
	}

	public void internalFrameOpened(InternalFrameEvent e) {
	}

	class MyCellRenderer extends JLabel implements ListCellRenderer {
		public MyCellRenderer() {
			setOpaque(true);
		}
		public Component getListCellRendererComponent(
			JList list,
			Object value,
			int index,
			boolean isSelected,
			boolean cellHasFocus) {
			if (value != null) {
				//System.out.println("Class: "+value.getClass());
				if (value instanceof File) {
					File file = (File) value;
					String fileName = file.getName();
					String newFileName = fileName.replaceAll("trafo_", "");
					String newestFileName =
						newFileName.replaceAll("\\.xsl", "");
					setText(newestFileName);
				} else {
					setText(value.toString());
				}
				setBackground(isSelected ? Color.orange : Color.white);
				setForeground(isSelected ? Color.white : Color.black);
			} else {
				setText("");
				setBackground(isSelected ? Color.red : Color.white);
				setForeground(isSelected ? Color.white : Color.black);
			}
			return this;

		}

	}
	public static void main(String[] args)  
  {
	final JFrame frame=new XSLTrafoView(I18n.get("TrafoView.WindowTitle"));
    frame.show();
  }  
}
