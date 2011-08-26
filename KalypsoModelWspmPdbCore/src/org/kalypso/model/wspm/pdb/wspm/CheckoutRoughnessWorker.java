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
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutRoughnessWorker
{
  private final TuhhWspmProject m_project;

  private final Roughness[] m_roughnesses;

  private final CheckoutDataMapping m_mapping;

  public CheckoutRoughnessWorker( final CheckoutDataMapping mapping, final TuhhWspmProject project, final Roughness[] roughnesses )
  {
    m_mapping = mapping;
    m_project = project;
    m_roughnesses = roughnesses;
  }

  public void execute( )
  {
    final IWspmClassification classificationMember = m_project.getClassificationMember();
    final IFeatureBindingCollection<IRoughnessClass> roughnessClassCollection = classificationMember.getRoughnessClassCollection();

    for( final Roughness roughness : m_roughnesses )
    {
      final String name = roughness.getId().getName();
      final IRoughnessClass roughnessClass = classificationMember.findRoughnessClass( name );
      if( roughnessClass == null )
        createRoughnessClass( roughness, roughnessClassCollection );
      else
        updateRoughnessClass( roughness, roughnessClass );
    }
  }

  private void createRoughnessClass( final Roughness roughness, final IFeatureBindingCollection<IRoughnessClass> collection )
  {
    final IRoughnessClass newClass = collection.addNew( IRoughnessClass.QNAME_FEATURE );
    newClass.setName( roughness.getId().getName() );
    final Color randomColor = ColorUtilities.random();
    final RGB randomRGB = org.kalypso.contribs.eclipse.swt.ColorUtilities.toRGB( randomColor );
    newClass.setColor( randomRGB );

    updateRoughnessProperties( roughness, newClass );

    m_mapping.addAddedFeatures( newClass );
  }

  private void updateRoughnessClass( final Roughness roughness, final IRoughnessClass roughnessClass )
  {
    updateRoughnessProperties( roughness, roughnessClass );

    m_mapping.addChangedFeatures( roughnessClass );
  }

  private void updateRoughnessProperties( final Roughness roughness, final IRoughnessClass roughnessClass )
  {
    roughnessClass.setDescription( roughness.getLabel() );
    roughnessClass.setKstValue( roughness.getKstValue() );
    roughnessClass.setKstValue( roughness.getKValue() );

    // TODO: we have no equivalent for the following properties
    // roughnessClass.setComment( roughness.getDescription() );
    // roughness.getSource();
    // roughness.getValidity();
  }
}
