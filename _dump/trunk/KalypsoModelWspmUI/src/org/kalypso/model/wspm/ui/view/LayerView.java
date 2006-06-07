package org.kalypso.model.wspm.ui.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer;


import de.belger.swtchart.layer.IChartLayer;

/**
 * @author Belger
 */
public class LayerView extends AbstractProfilViewPart
{
  private final ScrolledCompositeCreator m_creator = new ScrolledCompositeCreator( null )
  {
    @Override
  
    protected Control createContents( final Composite sc, final int style )
    {
      final Group group = new Group( sc, style );
      group.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      group.setLayout( new GridLayout() );

      return group;
    }
  };

  private IProfilChartLayer m_activeLayer;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    final Control control = m_creator.getContentControl();
    if( control != null )
      control.setFocus();
  }

  @Override
  protected Control createContent( final Composite parent )
  {
    m_creator.createControl( parent, SWT.NONE, SWT.NONE );
    onProfilViewDataChanged();

    onProfilViewDataChanged();

    return m_creator.getScrolledComposite();
  }

  @Override
  protected void saveState( )
  {
    // nothing to do
  }

  public void onProfilViewDataChanged( )
  {
    final ProfilViewData viewData = getViewData();
    final IChartLayer activeLayer = viewData == null ? null : viewData.getActiveLayer();

    final Shell shell = getSite().getShell();
    if( shell != null && !shell.isDisposed() )
    {
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !shell.isDisposed() )
            setLayer( activeLayer );
        }
      } );
    }
  }

  protected void setLayer( final IChartLayer layer )
  {
    final IProfilChartLayer profilLayer = layer instanceof IProfilChartLayer ? (IProfilChartLayer)layer
        : null;

    final Group group = (Group)m_creator.getContentControl();
    if( m_activeLayer == layer && group.getChildren().length > 0 )
      return;

    for( final Control c : group.getChildren() )
    {
      if( !c.isDisposed() )
        c.dispose();
    }
    group.setText( "" );

    if( layer != null )
    {
      group.setText( layer.toString() );

      final ProfilchartEditor editor = getProfilchartEditor();
      final IProfilEventManager pem = editor.getProfilEventManager();
      final IProfilView panel = profilLayer.createLayerPanel( pem, getViewData() );

      if( panel != null )
      {
        final Control control = panel.createControl( group, SWT.NONE );
        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }
      else
        new Label( group, SWT.NONE | SWT.WRAP );

      m_activeLayer = profilLayer;
    }
    else
    {
      // alles gescheitert -> Meldung
      final Label label = new Label( group, SWT.BORDER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "<Keine Legende vorhanden>" );
    }

    group.layout();
    m_creator.updateControlSize( false );
  }
}
