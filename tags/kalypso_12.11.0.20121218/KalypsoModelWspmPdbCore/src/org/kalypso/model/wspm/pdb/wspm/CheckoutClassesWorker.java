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

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;

/**
 * @author Gernot Belger
 */
public class CheckoutClassesWorker
{
  private final ICoefficients m_coefficients;

  private final CheckoutDataMapping m_mapping;

  private final GafCodes m_codes;

  public CheckoutClassesWorker( final GafCodes codes, final ICoefficients coefficients, final CheckoutDataMapping mapping )
  {
    m_codes = codes;
    m_coefficients = coefficients;
    m_mapping = mapping;
  }

  public void execute( final IProgressMonitor monitor )
  {
    final IWspmClassification classification = getOrCreateClassification();

    new CheckoutCodeWorker( m_mapping, classification, m_codes ).execute();
    new CheckoutRoughnessWorker( m_mapping, classification, m_coefficients.getAllRoughness() ).execute();
    new CheckoutVegetationWorker( m_mapping, classification, m_coefficients.getAllVegetation() ).execute();
    new CheckoutStylesWorker( m_mapping, classification, m_coefficients.getAllStyleArrays() ).execute();
    new CheckoutPartTypesWorker( m_mapping, classification, m_coefficients.getPartTypes() ).execute();

    monitor.done();
  }

  private IWspmClassification getOrCreateClassification( )
  {
    final TuhhWspmProject project = m_mapping.getProject();
    final IWspmClassification classification = project.getClassificationMember();
    if( classification != null )
      return classification;

    final IWspmClassification newClassification = project.createClassificationMember();

    m_mapping.addAddedFeatures( newClassification );

    return newClassification;
  }
}