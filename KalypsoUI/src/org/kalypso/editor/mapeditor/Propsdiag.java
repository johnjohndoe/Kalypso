package org.kalypso.editor.mapeditor;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.xml.types.LayerType;

/**
 * @author belger
 */
public class Propsdiag extends Dialog // implements SelectionListener
{
//  private ILoader[] m_loader = null;
//
//  private StackLayout m_stackLayout;
//
//  private Combo m_combo;
//
//  private final Map m_panelMap = new HashMap();
//
//  private Composite m_stack;
//
//  private Composite m_c;
//
//  private String[] m_types;
//
  private LayerType m_layer;

  public Propsdiag( final Shell parentShell, final LayerType layer )
  {
    super( parentShell );

    m_layer = layer;
    m_layer.getClass();
  }
//
//  protected void cancelPressed()
//  {
//    super.cancelPressed();
//  }
//
//  public boolean close()
//  {
//    m_combo.removeSelectionListener( this );
//
//    return super.close();
//  }
//
//  protected Control createDialogArea( Composite parent )
//  {
//    m_c = (Composite)super.createDialogArea( parent );
//
//    final RowLayout rowLayout = new RowLayout();
//    rowLayout.wrap = false;
//    rowLayout.pack = false;
//    rowLayout.justify = true;
//    rowLayout.type = SWT.VERTICAL;
//    rowLayout.marginLeft = 5;
//    rowLayout.marginTop = 5;
//    rowLayout.marginRight = 5;
//    rowLayout.marginBottom = 5;
//    rowLayout.spacing = 5;
//    m_c.setLayout( rowLayout );
//
//    m_combo = new Combo( m_c, SWT.READ_ONLY | SWT.BORDER );
//    
//    final ILoaderFactory loaderFactory = KalypsoGisPlugin.getDefault().getLoaderFactory();
//    m_types = loaderFactory.getTypes();
//    m_loader = new ILoader[m_types.length];
//
//    int selectIndex = 0;
//
//    for( int i = 0; i < m_types.length; i++ )
//    {
//      try
//      {
//        m_loader[i] = loaderFactory.getLoaderInstance( m_types[i], getClass().getClassLoader() );
//
//        if( m_layer.getType().equals( m_types[i] ) )
//        {
//          selectIndex = i;
//          m_loader[i].setSource( m_layer.getSource() );
//        }
//
//        m_combo.add( m_loader[i].getDescription() );
//      }
//      catch( final FactoryException e )
//      {
//        // TODO: error handling
//        e.printStackTrace();
//      }
//      catch( final LoaderException e )
//      {
//        // TODO: error handling
//
//        e.printStackTrace();
//      }
//    }
//
//    m_stack = new Composite( m_c, SWT.EMBEDDED );
//    
//    m_stackLayout = new StackLayout();
//    m_stack.setLayout( m_stackLayout );
//
//    m_combo.addSelectionListener( this );
//    m_combo.select( selectIndex );
//    
//    widgetSelected( null );
//
//    return m_c;
//  }
//
//  protected void okPressed()
//  {
//    m_layer.setType( m_types[m_combo.getSelectionIndex()] );
//    m_layer.setSource( m_loader[m_combo.getSelectionIndex()].getSource() );
//    
//    super.okPressed();
//  }
//
//  /**
//   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
//   */
//  public void widgetSelected( final SelectionEvent e )
//  {
//    try
//    {
//      final int index = m_combo.getSelectionIndex();
//      Composite c = (Composite)m_panelMap.get( m_loader[index] );
//      if( c == null )
//      {
//        c = (Composite)m_loader[index].createControl( m_stack );
//        m_panelMap.put( m_loader[index], c );
//      }
//
//      m_stackLayout.topControl = c;
//      m_c.layout();
//      m_stack.layout();
//    }
//    catch( final LoaderException e1 )
//    {
//      // TODO: error handling
//      e1.printStackTrace();
//    }
//  }
//
//  /**
//   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
//   */
//  public void widgetDefaultSelected( final SelectionEvent e )
//  {
//    // nix tun
//    System.out.println( "Default selection!" );
//  }
}