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
package org.kalypso.model.wspm.pdb.wspm;

import java.awt.Color;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutVegetationWorker
{
  private final CheckoutDataMapping m_mapping;

  private final Vegetation[] m_vegetations;

  private final IWspmClassification m_classification;

  public CheckoutVegetationWorker( final CheckoutDataMapping mapping, final IWspmClassification classification, final Vegetation[] vegetation )
  {
    m_mapping = mapping;
    m_classification = classification;
    m_vegetations = vegetation;
  }

  public void execute( )
  {
    final IFeatureBindingCollection<IVegetationClass> vegetationClassCollection = m_classification.getVegetationClassCollection();

    for( final Vegetation vegetation : m_vegetations )
    {
      final String name = vegetation.getId().getName();
      final IVegetationClass vegetationClass = m_classification.findVegetationClass( name );
      if( vegetationClass == null )
        createVegetationClass( vegetation, vegetationClassCollection );
      else
        updateVegetationClass( vegetation, vegetationClass );
    }
  }

  private void createVegetationClass( final Vegetation vegetation, final IFeatureBindingCollection<IVegetationClass> vegetationClassCollection )
  {
    final IVegetationClass newClass = vegetationClassCollection.addNew( IVegetationClass.FEATURE_VEGETATION_CLASS );
    newClass.setName( vegetation.getId().getName() );
    final Color randomColor = ColorUtilities.random();
    final RGB randomRGB = org.kalypso.contribs.eclipse.swt.ColorUtilities.toRGB( randomColor );
    newClass.setColor( randomRGB );

    updateVegetationProperties( vegetation, newClass );

    m_mapping.addAddedFeatures( newClass );
  }

  private void updateVegetationClass( final Vegetation vegetation, final IVegetationClass vegetationClass )
  {
    final boolean changed = updateVegetationProperties( vegetation, vegetationClass );
    if( changed )
      m_mapping.addChangedFeatures( vegetationClass );
  }

  private boolean updateVegetationProperties( final Vegetation vegetation, final IVegetationClass vegetationClass )
  {
    return new CheckoutVegetationUpdater( vegetation, vegetationClass ).update();
  }
}
