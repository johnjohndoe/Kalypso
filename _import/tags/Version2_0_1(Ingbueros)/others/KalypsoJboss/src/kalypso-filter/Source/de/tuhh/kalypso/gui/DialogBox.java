package de.tuhh.kalypso.gui;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.util.Iterator;

import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import de.tuhh.kalypso.data.Wc;
import de.tuhh.kalypso.data.WcTable;

/** The class DialogBox.java creates form a set of watercourses a wcList. (not
 * finished yet ).
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version DialogBox.java,v 1.0 2002/07/01
 */


public class DialogBox extends JDialog
{
	/** The constant holding the title of the frame.
	 * @param BOX_TITLE*/
	static final String BOX_TITLE = "Kalypso";
	JList myList;
	
	public DialogBox ( WcTable theWcList )
	{
		super( (Frame)null, true );
		
		setSize( 500, 500 );
		String[] choise = createArray( theWcList );
		
		JList myList = new JList( choise );
		
		Container c = getContentPane();
		c.setLayout( new BorderLayout() );
		
		c.add( myList, BorderLayout.NORTH );
		/*
		 WindowListener wndCloser = new WindowAdapter()
		 {
		 public void windowClosing( WindowEvent e )
		 {
		 setVisible( false );
		 dispose();
		 }
		 };
		 addWindowListener( wndCloser );
		 */
		setDefaultCloseOperation( JDialog.DISPOSE_ON_CLOSE );
		
		/*		String input = ( String ) JOptionPane.showInputDialog(DialogBox.this, "Please assign the state to the watercours index ",
		 BOX_TITLE, JOptionPane.INFORMATION_MESSAGE );
		 System.out.println( "You assigned " + input + " to the Watercourse " );
		 */
	}
	/** Ths inner class handles the choices made ( user ) from the wcList.*/
	public class ChoiceListener implements ListSelectionListener
	{
		/** This method convets the users choice form an item of the List to a string .
		 * @param e The ListSelectionEvent that causes the list to change.
		 */
		public void valueChanged( ListSelectionEvent e )
		{
			myList.getSelectedValue().toString();
			
		}
	}// inner class ChoiceListener
	/** This method creates an array of a WcTable with all wcIndex in the array.
	 * @param theWcList A HashSet to be converted to a array.*/
	public String[] createArray ( WcTable theWcList )
	{
		String[] array = new String[theWcList.size()];
		Iterator it = theWcList.iterator();
		for ( int i = 0 ; i < theWcList.size(); i++ )
		{
			Wc element = ( Wc )it.next();
			array[i] = element.toString();
		}
		return array;
	}// crateArray
}// DialogBox


