package com.bce.datacenter.dcbrowser;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.bce.datacenter.db.IngresDatabase;
import com.bce.datacenter.db.common.Level;
import com.bce.datacenter.db.timeseries.Channel;
import com.bce.datacenter.db.timeseries.Timeserie;

/**
 * Timeseries Browser
 * 
 * @author schlienger
 */
public class DCBrowser extends JFrame
{
  protected final DCTableModel m_tableModel;

  private final DCTreeModel m_treeModel;

  protected final JTable m_table;

  private final JTree m_tree;

  /**
   * Creates a new DCBrowser object.
   * @param ingres
   */
  public DCBrowser( final IngresDatabase ingres )
  {
    final Level root = Level.getRoot( ingres.getConnection() );

    m_treeModel = new DCTreeModel( root );
    m_tree = new JTree( m_treeModel );
    
    m_tableModel = new DCTableModel( root );
    m_table = new JTable( m_tableModel );

    // configure tree
    m_tree.getSelectionModel().setSelectionMode(
        TreeSelectionModel.SINGLE_TREE_SELECTION );
    m_tree.putClientProperty( "JTree.lineStyle", "Angled" );
    m_tree.setShowsRootHandles( true );
    m_tree.setEditable( false );
    m_tree.addTreeSelectionListener( new DirSelectionListener() );

    m_table.addMouseListener( new EltSelectionListener() );

    JPanel panel = new JPanel( new BorderLayout() );
    panel.add( new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(
        m_tree ), new JScrollPane( m_table ) ), BorderLayout.CENTER );
    this.getContentPane().add( panel );

    setSize( 500, 400 );
    setTitle( "Zeitreihen-Browser" );
  }

  public static void main( String[] args )
  {
    // here we establish the connection with the database...
    try
    {
      UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );

      DCBrowser dcb = new DCBrowser(
          new IngresDatabase(
              "jdbc:edbc://PC083:LP7/BCE_PC083::dbabwb2/INGRES", "ingres",
              "ingres" ) );

      dcb.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
      dcb.setVisible( true );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      System.exit( 1 );
    }
  }

  /**
   * get the tree node at the end of the path
   * 
   * @param path
   *          a tree path
   * 
   * @return ?
   */
  public Level pathToLevel( TreePath path )
  {
    return (Level) (path.getLastPathComponent());
  }

  class DirSelectionListener implements TreeSelectionListener
  {
    public void valueChanged( TreeSelectionEvent event )
    {
      Level level = pathToLevel( event.getPath() );

      if( level != null )
        m_tableModel.setCurrentLevel( level );
    }
  }

  class EltSelectionListener extends JPopupMenu implements MouseListener
  {
    /**
     * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
     */
    public void mouseClicked( MouseEvent e )
    {
      checkPopup( e );
    }

    /**
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed( MouseEvent e )
    {
      checkPopup( e );
    }

    /**
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased( MouseEvent e )
    {
      checkPopup( e );
    }

    /**
     * Checks if this is a popup trigger and shows popup menu
     * 
     * @param e -
     */
    private void checkPopup( MouseEvent e )
    {
      this.removeAll();

      if( e.isPopupTrigger() )
      {
        int pos = m_table.getSelectedRow();

        Level level = m_tableModel.getCurrentLevel();

        List objects = level.getObjects();

        if( pos >= 0 && pos < objects.size() )
        {
          Channel channel = (Channel) objects.get( pos );

          List tss = channel.getTimeseries();

          Iterator it = tss.iterator();

          while( it.hasNext() )
          {
            add( new TimeserieAction( (Timeserie) it.next() ) );
          }
        }

        show( m_table, e.getX(), e.getY() );
      }
    }

    /**
     * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
     */
    public void mouseEntered( MouseEvent e )
    {
      // nothing
    }

    /**
     * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
     */
    public void mouseExited( MouseEvent e )
    {
      // nothing
    }
  }

  private static class TimeserieAction extends AbstractAction
  {
    private final String m_table;

    public TimeserieAction( Timeserie t )
    {
      super( t.toString() );

      m_table = t.getDataTableName();

      putValue( Action.NAME, t.toString() );
    }

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed( ActionEvent e )
    {
      // TODO retrieve the table name from here
      System.out.println( m_table );
    }
  }
}