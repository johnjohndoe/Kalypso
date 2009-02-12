package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;

import tools.MyFileFilter;
import tools.RasterTools;
import converter.ArcGridConverter;

public class SubstractGridsView extends JInternalFrame implements ActionListener
{

  File input1File = null;

  File input2File = null;

  File targetFile = null;

  JLabel lbGrid1 = new JLabel( "Grid 1:" );

  JButton btInput1 = new JButton( "Choose..." );

  JLabel lbGrid2 = new JLabel( "Grid 2:" );

  JButton btInput2 = new JButton( "Choose..." );

  JLabel lbTargetFile = new JLabel( "Target File:" );

  JButton btTargetFile = new JButton( "Choose..." );

  JButton btStart = new JButton( "Start" );

  JFileChooser fileChooser = new JFileChooser();

  public SubstractGridsView()
  {
    super( "SubstractGrids", true, true, true, true );
    setBounds( 0, 0, 600, 150 );
    initGUI();
    updateStatus();
    show();
  }

  private void initGUI()
  {

    btInput1.setActionCommand( "input1" );
    btInput1.addActionListener( this );
    btInput1.setBackground( Color.white );
    btInput1.setBorderPainted( false );
    btInput1.setMargin( new Insets( 0, 0, 0, 0 ) );

    btInput2.setActionCommand( "input2" );
    btInput2.addActionListener( this );
    btInput2.setBackground( Color.white );
    btInput2.setBorderPainted( false );
    btInput2.setMargin( new Insets( 0, 0, 0, 0 ) );

    btTargetFile.setActionCommand( "targetFile" );
    btTargetFile.addActionListener( this );
    btTargetFile.setBackground( Color.white );
    btTargetFile.setBorderPainted( false );
    btTargetFile.setMargin( new Insets( 0, 0, 0, 0 ) );

    btStart.setActionCommand( "start" );
    btStart.addActionListener( this );

    getContentPane().setLayout( new BorderLayout() );

    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout( new GridLayout( 3, 2 ) );
    buttonPanel.add( lbGrid1 );
    buttonPanel.add( btInput1 );
    buttonPanel.add( lbGrid2 );
    buttonPanel.add( btInput2 );
    buttonPanel.add( lbTargetFile );
    buttonPanel.add( btTargetFile );

    JPanel startPanel = new JPanel();
    startPanel.setLayout( new BorderLayout() );
    startPanel.add( btStart, BorderLayout.CENTER );

    getContentPane().add( buttonPanel, BorderLayout.CENTER );
    getContentPane().add( startPanel, BorderLayout.SOUTH );
  }

  public void updateStatus()
  {
    if( input1File != null && input2File != null && targetFile != null && input1File.exists()
        && input2File.exists() )
    {
      btStart.setEnabled( true );
    }
    else
    {
      btStart.setEnabled( false );
    }
  }

  File currentFile = new File( "." );

  private int openFileChooser()
  {
    fileChooser.setFileSelectionMode( JFileChooser.FILES_ONLY );
    fileChooser.setCurrentDirectory( currentFile );
    fileChooser.setFileFilter( new MyFileFilter( new String[]
    { "asc" } ) );
    int returnValue = fileChooser.showOpenDialog( this );
    currentFile = fileChooser.getSelectedFile();
    return returnValue;
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed( ActionEvent e )

  {
    String action = e.getActionCommand();

    if( "input1".equals( action ) )
    {
      int returnVal = openFileChooser();
      if( returnVal == JFileChooser.APPROVE_OPTION )
      {
        input1File = fileChooser.getSelectedFile();
        btInput1.setText( input1File.getName() );
      }
    }

    if( "input2".equals( action ) )
    {
      int returnVal = openFileChooser();
      if( returnVal == JFileChooser.APPROVE_OPTION )
      {
        input2File = fileChooser.getSelectedFile();
        btInput2.setText( input2File.getName() );
      }
    }

    if( "targetFile".equals( action ) )
    {
      int returnVal = openFileChooser();
      if( returnVal == JFileChooser.APPROVE_OPTION )
      {
        File tempFile = fileChooser.getSelectedFile();
        if( ( tempFile.getName() ).indexOf( "." ) == -1 )
          targetFile = new File( tempFile.getParent() + "//" + tempFile.getName() + ".asc" );
        else
          targetFile = tempFile;
        btTargetFile.setText( targetFile.getName() );
      }
    }
    if( "start".equals( action ) )
    {
      new Thread()
      {
        public void run()
        {
          try
          {
            ArcGridConverter converter = new ArcGridConverter();
            LogView.println( "Import " + input1File.getName() + "..." );
            RectifiedGridCoverage grid1 = converter.importGridArc( input1File );
            LogView.println( "Import " + input2File.getName() + "..." );
            RectifiedGridCoverage grid2 = converter.importGridArc( input2File );
            LogView.println( "Substraction..." );
            RectifiedGridCoverage resultGrid = RasterTools.substractGrids( grid1, grid2 );
            LogView.println( "...Substraction finished. Write ResultFile..." );
            converter.exportGridArc( targetFile, resultGrid );
            LogView.println( "...finished" );
          }
          catch( Exception e1 )
          {
            System.out.println( e1 );
          }
        }
      }.start();
      this.dispose();
    }

    updateStatus();

  }

}