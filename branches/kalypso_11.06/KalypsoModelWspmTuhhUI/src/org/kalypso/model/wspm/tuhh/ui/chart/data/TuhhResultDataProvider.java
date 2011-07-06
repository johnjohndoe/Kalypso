package org.kalypso.model.wspm.tuhh.ui.chart.data;

import java.math.BigDecimal;
import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kimwerner
 */
public final class TuhhResultDataProvider implements IWspLayerData
{

  private final IWspmResultNode m_results;

  private final Set<TuhhResultDataElement> m_activeElements = new LinkedHashSet<TuhhResultDataElement>();

  private final IDialogSettings m_settings;

  private final String m_settingId;

  public TuhhResultDataProvider( final IWspmResultNode results, final String settingId )
  {
    m_results = results;
    m_settingId = settingId;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );

    initResults( m_results );
  }

  @Override
  public double searchValue( final Object element, final BigDecimal station ) throws Exception
  {
    if( element instanceof TuhhResultDataElement )
    {
      final TuhhResultDataElement wspElement = (TuhhResultDataElement) element;
      return wspElement.getValue( station );
    }

    return Double.NaN;
  }

  @Override
  public Object getInput( ) throws Exception
  {
    return m_results;
  }

  private void initResults( final IWspmResultNode node )
  {
    if( node == null )
      return;

    initActive( node );

    final IWspmResultNode[] childNodes = node.getChildResults();
    for( final IWspmResultNode child : childNodes )
    {

      initResults( child );
    }
  }

  private void initActive( final IWspmResultNode resultNode )
  {
    final String id = resultNode.getName();
    final String settingsName = getSettingsName();
    final IDialogSettings section = DialogSettingsUtils.getSection( m_settings, settingsName );
    final boolean isActive = section.getBoolean( id );
    if( isActive )
    {
      m_activeElements.add( new TuhhResultDataElement( resultNode ) );
    }
  }

  @Override
  public TuhhResultDataElement[] getActiveElements( ) throws Exception
  {
    return m_activeElements.toArray( new TuhhResultDataElement[m_activeElements.size()] );
  }

  @Override
  public void activateElements( final Object[] elements ) throws Exception
  {
    m_activeElements.clear();

    for( final Object element : elements )
    {
      if( element instanceof IWspmResultNode )
      {
        m_activeElements.add( new TuhhResultDataElement( (IWspmResultNode) element ) );
      }
      else if( element instanceof TuhhResultDataElement )
      {
        m_activeElements.add( (TuhhResultDataElement) element );
      }
    }

    if( m_settings == null )
      return;

    /* Recreate the sub-section */
    final String settingsBase = getSettingsName();
    final IDialogSettings section = m_settings.addNewSection( settingsBase );
    for( final TuhhResultDataElement resultNode : m_activeElements )
    {
      section.put( resultNode.getId(), true );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.layer.IWspLayerData#createLabelProvider()
   */
  @Override
  public ILabelProvider createLabelProvider( )
  {
    return new TuhhResultDataElementLabelProvider();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.layer.IWspLayerData#createContentProvider()
   */
  @Override
  public ITreeContentProvider createContentProvider( )
  {
    return new TuhhResultDataElementContentProvider();
  }

  /**
   * We use the gml context as base name for the settings. That means, we remember the settings per wspm-model.
   */
  private String getSettingsName( )
  {
    final Object object = m_results.getObject();
    if( object instanceof Feature )
    {
      final Feature feature = (Feature) object;
      final GMLWorkspace workspace = feature.getWorkspace();
      if( workspace != null )
      {
        final URL context = workspace.getContext();
        if( context != null )
          return m_settingId + context.toExternalForm();
      }
    }

    return m_settingId;
  }
}