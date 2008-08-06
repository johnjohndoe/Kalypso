/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editor.mapeditor.commands;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * Selects the next feature from the current active theme of the map.
 * 
 * @author Gernot Belger
 */
public class SelectNextFeatureHandler extends AbstractHandler implements IExecutableExtension
{
  /** Move forward (<code>true</code>) or backward (<code>false</code>) */
  private boolean m_forward = false;

  /** If <code>true</code>, jump to first/last index if maxIndex or 0 is exceeded. */
  private boolean m_rotate = false;

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  @SuppressWarnings("unchecked")
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      Map<String, String> m_map = (Map<String, String>) data;
      m_forward = Boolean.valueOf( m_map.get( "forward" ) ).booleanValue();
      m_rotate = Boolean.valueOf( m_map.get( "rotate" ) ).booleanValue();
    }
  }

  /**
   * @see org.eclipse.ui.commands.IHandler#execute(java.util.Map)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final MapPanel mapPanel = (MapPanel) context.getVariable( "mapPanel" );
    if( mapPanel == null )
      throw new ExecutionException( "MapPanel not set" );

    mapPanel.getSelection();
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return null;

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( !(activeTheme instanceof IKalypsoFeatureTheme) )
      return null;

    final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) activeTheme;
    final FeatureList featureList = featureTheme.getFeatureList();

    if( featureList.isEmpty() )
      return null;

    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final Object firstElement = selectionManager.getFirstElement();

    final int index = featureList.indexOf( firstElement );

    final int newIndex = computeNewIndex( index, featureList.size() - 1 );

    final Feature featureToSelect = (Feature) featureList.get( newIndex );

    final EasyFeatureWrapper wrapperToSelect = new EasyFeatureWrapper( featureTheme.getWorkspace(), featureToSelect, featureToSelect.getParent(), featureToSelect.getParentRelation() );

    final Feature[] toRemove = FeatureSelectionHelper.getFeatures( selectionManager );

    selectionManager.changeSelection( toRemove, new EasyFeatureWrapper[] { wrapperToSelect } );

    return null;
  }

  /**
   * @param index
   *          The current index
   * @param maxIndex
   *          The maximal possible index (the minimal possible index is assumed to be 0)
   */
  private int computeNewIndex( final int index, final int maxIndex )
  {
    if( index < 0 || index > maxIndex )
      return m_forward ? 0 : maxIndex;

    final int nextIndex = m_forward ? index + 1 : index - 1;

    if( nextIndex <= 0 )
      return m_rotate ? maxIndex : 0;

    if( nextIndex > maxIndex )
      return m_rotate ? 0 : maxIndex;

    return nextIndex;
  }
}
