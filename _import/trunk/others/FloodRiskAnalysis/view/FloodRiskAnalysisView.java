package view;

import javax.swing.DefaultDesktopManager;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;

/**
 * Main class for starting a FloodRiskAnalysis with a user interface
 * 
 * @author N. Peiler
 *  
 */
public class FloodRiskAnalysisView extends JFrame
{

  //	 listener for menu events
  private MenuHandler menuHandler = null;

  public static FloodRiskAnalysisView floodRiskFrame = null;

  public JDesktopPane desktop = null;

  public JMenuItem menuItem_open = null;

  public JMenuItem menuItem_close = null;

  public JMenuItem menuItem_waterlevel = null;

  public JMenuItem menuItem_landuse = null;

  public JMenuItem menuItem_adminUnit = null;

  public JMenuItem menuItem_damage = null;

  public JMenuItem menuItem_annualDamage = null;

  public JMenuItem menuItem_floodRisk = null;

  public JMenuItem menuItem_rasterView = null;

  public JMenuItem menuItem_statisticView = null;

  /**
   * constructs a FloodRiskAnalysisView
   * 
   * @param header
   *          header of the Frame
   */
  public FloodRiskAnalysisView( String header )
  {
    super( header );
    // add listener for closing the frame/application
    addWindowListener( new java.awt.event.WindowAdapter()
    {
      public void windowClosing( java.awt.event.WindowEvent evt )
      {
        System.exit( 0 );
      }
    } );
    setBounds( 0, 0, 700, 500 );
    menuHandler = new MenuHandler( this );
    initGUI();
    show();
    floodRiskFrame = this;
  }

  private void initGUI()
  {
    initMenuBar();
    desktop = new JDesktopPane();
    desktop.setDesktopManager( new DefaultDesktopManager() );
    setContentPane( desktop );
    desktop.add( LogView.getInstance() );
  }

  private void initMenuBar()
  {
    JMenuBar menuBar = new JMenuBar();

    // file menu
    // project menu
    JMenu menu_project = new JMenu( "Project" );
    menuItem_open = menu_project.add( "open" );
    menuItem_open.addActionListener( menuHandler );
    menuItem_open.setActionCommand( "open project" );
    menuItem_open.setEnabled( true );
    menuItem_close = menu_project.add( "close" );
    menuItem_close.addActionListener( menuHandler );
    menuItem_close.setActionCommand( "close project" );
    menuItem_close.setEnabled( false );
    menuBar.add( menu_project );

    // convert menu
    JMenu menu_convert = new JMenu( "Convert" );
    menuItem_waterlevel = menu_convert.add( "waterlevel" );
    menuItem_waterlevel.addActionListener( menuHandler );
    menuItem_waterlevel.setActionCommand( "convert waterlevel" );
    menuItem_waterlevel.setEnabled( false );
    menuItem_landuse = menu_convert.add( "landuse" );
    menuItem_landuse.addActionListener( menuHandler );
    menuItem_landuse.setActionCommand( "convert landuse" );
    menuItem_landuse.setEnabled( false );
    menuItem_adminUnit = menu_convert.add( "administrationUnit" );
    menuItem_adminUnit.addActionListener( menuHandler );
    menuItem_adminUnit.setActionCommand( "convert adminUnit" );
    menuItem_adminUnit.setEnabled( false );
    menuBar.add( menu_convert );

    // calculate menu
    JMenu menu_calculate = new JMenu( "Calculate" );
    menuItem_damage = menu_calculate.add( "damage" );
    menuItem_damage.addActionListener( menuHandler );
    menuItem_damage.setActionCommand( "calculate damage" );
    menuItem_damage.setEnabled( false );
    menuItem_annualDamage = menu_calculate.add( "annual damage" );
    menuItem_annualDamage.addActionListener( menuHandler );
    menuItem_annualDamage.setActionCommand( "calculate annualDamage" );
    menuItem_annualDamage.setEnabled( false );
    menuItem_floodRisk = menu_calculate.add( "floodrisk" );
    menuItem_floodRisk.addActionListener( menuHandler );
    menuItem_floodRisk.setActionCommand( "calculate floodRisk" );
    menuItem_floodRisk.setEnabled( false );
    menuBar.add( menu_calculate );

    // open menu
    JMenu menu_open = new JMenu( "Open" );
    menuItem_rasterView = menu_open.add( "Raster View" );
    menuItem_rasterView.addActionListener( menuHandler );
    menuItem_rasterView.setActionCommand( "open rasterView" );
    menuItem_rasterView.setEnabled( false );
    menuItem_statisticView = menu_open.add( "Statistic View" );
    menuItem_statisticView.addActionListener( menuHandler );
    menuItem_statisticView.setActionCommand( "open statisticView" );
    menuItem_statisticView.setEnabled( false );
    menuBar.add( menu_open );

    // add menu bar to the frame
    setJMenuBar( menuBar );
  }

  public static void main( String[] args ) throws Exception
  {
    ITypeRegistry typeRegistry = TypeRegistrySingleton.getTypeRegistry();
    typeRegistry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
    typeRegistry.registerTypeHandler( new RangeSetTypeHandler() );
    new FloodRiskAnalysisView( "FloodRiskAnalysis" );
  }
}