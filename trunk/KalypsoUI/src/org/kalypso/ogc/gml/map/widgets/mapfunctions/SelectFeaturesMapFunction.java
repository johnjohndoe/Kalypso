/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets.mapfunctions;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.changers.IFeatureSelectionChanger;
import org.kalypso.ogc.gml.map.widgets.providers.IFeaturesProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

/**
 * Selects features in a map. Chooses from all themes of a certain QName (and its substitutes).
 * 
 * @author Holger Albert
 */
public class SelectFeaturesMapFunction implements IRectangleMapFunction
{
  /**
   * The default radius for selecting features.
   */
  public static final int DEFAULT_RADIUS = 20;

  /**
   * Features within this radius are selected.
   */
  private final int m_radius;

  /**
   * Changes the selection.
   */
  private final IFeatureSelectionChanger m_selectionChanger;

  /**
   * This class provides the features. It could do it on several ways (e.g. via QName or if it has geometrys).
   */
  private final IFeaturesProvider m_featuresProvider;

  /**
   * The selection manager holds the current selection.
   */
  private final IFeatureSelectionManager m_manager;

  /**
   * The constructor.
   * 
   * @param radius
   *            The radius in which the features should be selected.
   * @param featuresProvider
   *            The provider of the features (e.g.
   *            {@link org.kalypso.informdss.manager.util.widgets.providers.QNameFeaturesProvider}).
   * @param selectionChanger
   *            The selection changer is responsible for really changing the selection in a specific way (e.g.
   *            {@link org.kalypso.informdss.manager.util.widgets.changers.SingleSelectionChanger}).
   * @param manager
   *            The selection manager, which holds the current selection.
   */
  public SelectFeaturesMapFunction( final int radius, final IFeaturesProvider featuresProvider, final IFeatureSelectionChanger selectionChanger, final IFeatureSelectionManager manager )
  {
    m_radius = radius;
    m_featuresProvider = featuresProvider;
    m_selectionChanger = selectionChanger;
    m_manager = manager;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IRectangleMapFunction#execute(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Rectangle)
   */
  public void execute( final MapPanel mapPanel, final Rectangle rectangle )
  {
    final EasyFeatureWrapper[] wrappers = m_featuresProvider.getFeatures( mapPanel );
    final EasyFeatureWrapper[] wrappersToSelect = MapfunctionHelper.findFeatureToSelect( mapPanel, rectangle, wrappers, m_radius );
    if( wrappersToSelect != null )
      m_selectionChanger.changeSelection( m_manager, wrappers, wrappersToSelect );
  }

  protected int getRadius( )
  {
    return m_radius;
  }

  protected IFeaturesProvider getFeaturesProvider( )
  {
    return m_featuresProvider;
  }

}