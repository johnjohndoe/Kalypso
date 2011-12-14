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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;

import org.kalypso.model.hydrology.binding._11_6.NAControl;
import org.kalypso.model.hydrology.binding._11_6.NAModellControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class ConverterData
{
  private final File m_baseDir;

  public ConverterData( final File baseDir )
  {
    m_baseDir = baseDir;
  }

  public NAModellControl loadControl( ) throws Exception
  {
    return loadModel( INaCalcCaseConstants.EXPERT_CONTROL_PATH );
  }

  public NAControl loadMetaControl( ) throws Exception
  {
    return loadModel( INaCalcCaseConstants.CALCULATION_GML_PATH );
  }

  public NaModell loadNaModel( ) throws Exception
  {
    return loadModel( INaProjectConstants.GML_MODELL_PATH );
  }

  @SuppressWarnings("unchecked")
  <F extends Feature> F loadModel( final String path ) throws Exception
  {
    final File file = new File( m_baseDir, path );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file, null );
    return (F) workspace.getRootFeature();
  }

  void saveModel( final Feature model, final String path ) throws IOException, GmlSerializeException
  {
    // REMARK: we assume that all files are 'UTF-8'.
    final File file = new File( m_baseDir, path );
    GmlSerializer.serializeWorkspace( file, model.getWorkspace(), "UTF-8" ); //$NON-NLS-1$
  }
}
