package org.kalypso.model.wspm.tuhh.ui.chart.data;

import java.math.BigDecimal;
import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
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
  private final Set<TuhhResultDataElement> m_activeElements = new LinkedHashSet<>();

  private final IDialogSettings m_settings;

  private final String m_settingId;

  private final IProfile m_profile;

  private final TuhhResultDataElement m_rootElement;

  public TuhhResultDataProvider( final IProfile profile, final IWspmResultNode results, final String settingId )
  {
    m_profile = profile;
    m_settingId = settingId;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );

    m_rootElement = new TuhhResultDataElement( null, results );

    final String settingsName = getSettingsName();
    final IDialogSettings section = DialogSettingsUtils.getSection( m_settings, settingsName );

    // FIXME: build all data element now and here...

    initResults( m_rootElement, section );
  }

  @Override
  public double searchValue( final Object element, final BigDecimal station )
  {
    final TuhhResultDataElement wspElement = (TuhhResultDataElement)element;
    return wspElement.getValue( station );
  }

  @Override
  public Object getInput( )
  {
    return this;
  }

  private void initResults( final TuhhResultDataElement element, final IDialogSettings section )
  {
    if( element == null )
      return;

    initActive( element, section );

    final TuhhResultDataElement[] children = element.getChildren();
    for( final TuhhResultDataElement child : children )
      initResults( child, section );
  }

  private void initActive( final TuhhResultDataElement element, final IDialogSettings section )
  {
    final String id = element.getId();
    final boolean isActive = section.getBoolean( id );
    if( isActive )
      m_activeElements.add( element );
  }

  @Override
  public TuhhResultDataElement[] getActiveElements( )
  {
    return m_activeElements.toArray( new TuhhResultDataElement[m_activeElements.size()] );
  }

  @Override
  public void activateElements( final Object[] elements )
  {
    m_activeElements.clear();

    for( final Object element : elements )
      m_activeElements.add( (TuhhResultDataElement)element );

    if( m_settings == null )
      return;

    /* Recreate the sub-section */
    final String settingsBase = getSettingsName();
    final IDialogSettings section = m_settings.addNewSection( settingsBase );
    for( final TuhhResultDataElement resultNode : m_activeElements )
      section.put( resultNode.getId(), true );
  }

  @Override
  public ILabelProvider createLabelProvider( )
  {
    return new TuhhResultDataElementLabelProvider();
  }

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
    final IWspmResultNode results = m_rootElement.getResultNode();
    final Object object = results.getObject();
    if( object instanceof Feature )
    {
      final Feature feature = (Feature)object;
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

  TuhhResultDataElement[] getRootElements( )
  {
    return m_rootElement.getChildren();
  }
}