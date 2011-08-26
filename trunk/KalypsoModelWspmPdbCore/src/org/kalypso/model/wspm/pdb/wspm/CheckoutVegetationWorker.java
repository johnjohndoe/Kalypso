/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutVegetationWorker
{
  private final TuhhWspmProject m_project;

  private final CheckoutDataMapping m_mapping;

  private final Vegetation[] m_vegetations;

  public CheckoutVegetationWorker( final CheckoutDataMapping mapping, final TuhhWspmProject project, final Vegetation[] vegetation )
  {
    m_mapping = mapping;
    m_project = project;
    m_vegetations = vegetation;
  }

  public void execute( )
  {
    final IWspmClassification classificationMember = m_project.getClassificationMember();
    final IFeatureBindingCollection<IVegetationClass> vegetationClassCollection = classificationMember.getVegetationClassCollection();

    for( final Vegetation vegetation : m_vegetations )
    {
      final String name = vegetation.getId().getName();
      final IVegetationClass vegetationClass = classificationMember.findVegetationClass( name );
      if( vegetationClass == null )
        createVegetationClass( vegetation, vegetationClassCollection );
      else
        updateVegetationClass( vegetation, vegetationClass );
    }
  }

  private void createVegetationClass( final Vegetation vegetation, final IFeatureBindingCollection<IVegetationClass> vegetationClassCollection )
  {
    final IVegetationClass newClass = vegetationClassCollection.addNew( IVegetationClass.QNAME_FEATURE );
    newClass.setName( vegetation.getId().getName() );
    final Color randomColor = ColorUtilities.random();
    final RGB randomRGB = org.kalypso.contribs.eclipse.swt.ColorUtilities.toRGB( randomColor );
    newClass.setColor( randomRGB );

    updateVegetationProperties( vegetation, newClass );

    m_mapping.addAddedFeatures( newClass );
  }

  private void updateVegetationClass( final Vegetation vegetation, final IVegetationClass vegetationClass )
  {
    updateVegetationProperties( vegetation, vegetationClass );

    m_mapping.addChangedFeatures( vegetationClass );
  }

  private void updateVegetationProperties( final Vegetation vegetation, final IVegetationClass vegetationClass )
  {
    vegetationClass.setDescription( vegetation.getLabel() );
    vegetationClass.setAx( vegetation.getAx() );
    vegetationClass.setAy( vegetation.getAy() );
    vegetationClass.setDp( vegetation.getDp() );

    // TODO: we have no equivalent for the following properties
    // vegetationClass.setComment( roughness.getDescription() );
    // vegetation.getSource();
    // vegetation.getValidity();
  }
}
