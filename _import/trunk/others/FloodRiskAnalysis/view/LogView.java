package view;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * View to inform the user about the actual process
 * 
 * @author N. Peiler
 *  
 */
public class LogView extends JInternalFrame implements ActionListener
{
  private JPanel panel = new JPanel( new GridBagLayout() );

  JTextArea jTextArea = new JTextArea();

  JScrollPane scroller = new JScrollPane();

  JButton jClear = new JButton( "Clear" );

  static LogView instance = null;

  private LogView( String title )
  {
    super( title );
    setResizable( true );
    setIconifiable( true );
    //setLocation(new Point(600,400));
    initMask();
    setVisible( true );
    setSize( 300, 300 );
  }

  public static LogView getInstance()
  {
    if( instance == null )
      instance = new LogView( "LogView" );
    return instance;
  }

  public static void print( String text )
  {
    LogView.getInstance().log( text );
  }

  public static void println( String text )
  {
    LogView.getInstance().logln( text );
  }

  public void log( String text )
  {
    if( text != null )
    {
      System.out.print( text );
      jTextArea.append( text );
      repaint();
    }
  }

  public void logln( String text )
  {
    if( text != null )
    {
      System.out.println( text );

      if( jTextArea.getRows() > 10 )
        jTextArea.setRows( 10 );

      jTextArea.append( text + "\n" );
      try
      {
        // always scroll to the end of the document in the jTextArea
        jTextArea.setCaretPosition( jTextArea.getDocument().getLength() );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
      repaint();
    }
  }

  private void initMask()
  {
    //add2ViewLastInRow(new JLabel("Label"));
    jTextArea.setText( "" );
    scroller.setViewportView( jTextArea );

    GridBagLayout layout = (GridBagLayout)panel.getLayout();
    GridBagConstraints layoutConstraints = new GridBagConstraints();
    layoutConstraints.fill = GridBagConstraints.BOTH;
    layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
    layoutConstraints.gridheight = 1;
    layoutConstraints.weightx = 0;
    layoutConstraints.weighty = 1;
    layout.setConstraints( scroller, layoutConstraints );
    panel.add( scroller );

    add2ViewLastInRow( jClear );

    jClear.addActionListener( this );
    jClear.setActionCommand( "clear" );
    getContentPane().add( panel );
  }

  // ActionListener
  public void actionPerformed( ActionEvent e )
  {
    String command = e.getActionCommand();

    if( "clear".equals( command ) )
    {
      jTextArea.setText( "" );
    }
  }

  public void add2View( JComponent component )
  {
    GridBagLayout layout = (GridBagLayout)panel.getLayout();
    GridBagConstraints layoutConstraints = new GridBagConstraints();
    layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
    layoutConstraints.gridwidth = 1;
    layoutConstraints.gridheight = 1;
    layoutConstraints.weightx = 0.5;
    layoutConstraints.weighty = 0;
    layout.setConstraints( component, layoutConstraints );
    panel.add( component );
  }

  public void add2ViewLastInRow( JComponent component )
  {
    GridBagLayout layout = (GridBagLayout)panel.getLayout();
    GridBagConstraints layoutConstraints = new GridBagConstraints();
    layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
    layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
    layoutConstraints.gridheight = 1;
    layoutConstraints.weightx = 0.5;
    layoutConstraints.weighty = 0;
    layout.setConstraints( component, layoutConstraints );
    panel.add( component );
  }

}