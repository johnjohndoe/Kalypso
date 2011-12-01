/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.conv;

import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;

/**
 * This factory returns a discretisation model converter (MeshConverter) to the specified file format (*.2d, *2.dm,
 * *.hmo)
 * 
 * @author felipe
 * 
 */
public class MeshConverterFactory
{
  private static final String _2D_EXTENSION = "*.2d";

  private static final String _2DM_EXTENSION = "*.2dm";

  private static final String _HMO_EXTENSION = "*.hmo";
  
  public MeshConverterFactory( )
  {

  }

  public I2DMeshConverter getConverter( final IFEDiscretisationModel1d2d discretisationModel, final IFlowRelationshipModel flowRelationshipModel, final ICalculationUnit calcUnit, IRoughnessClsCollection roughnessModel, final RestartNodes restartNodes, final boolean exportRequested, final boolean exportMiddleNode, final IGeoLog log, final String extension ) 
  {
    I2DMeshConverter converter = null;
    if( extension.equals( _2D_EXTENSION ) )
    {
      converter = new Gml2RMA10SConv( discretisationModel, flowRelationshipModel, calcUnit, roughnessModel, restartNodes, exportRequested, exportMiddleNode, log );
    }
    else if( extension.equals( _2DM_EXTENSION ) )
    {
      converter = new Gml2SMSConv( discretisationModel, roughnessModel );
    }

    else if( extension.equals( _HMO_EXTENSION ) )
    {
      converter = new GmlMesh2HmoConverter( discretisationModel );
    }

    return converter;
  }
  
  public static boolean supportMidSideNodes( final String extension )
  {
    boolean support = false;
    if( extension.equals( _2D_EXTENSION ) )
    {
      support = Gml2RMA10SConv.SUPPORT_MIDSIDE_NODES;
    }
    else if( extension.equals( _2DM_EXTENSION ) )
    {
      support = Gml2SMSConv.SUPPORT_MIDSIDE_NODES;
    }

    else if( extension.equals( _HMO_EXTENSION ) )
    {
      support = GmlMesh2HmoConverter.SUPPORT_MIDSIDE_NODES;
    }
    
    return support;
  }
  
  public static boolean supportFlowResistanceClasses( final String extension )
  {
    boolean support = false;
    if( extension.equals( _2D_EXTENSION ) )
    {
      support = Gml2RMA10SConv.SUPPORT_FLOW_RESISTANCE_CLASSES;
    }
    else if( extension.equals( _2DM_EXTENSION ) )
    {
      support = Gml2SMSConv.SUPPORT_FLOW_RESISTANCE_CLASSES;
    }

    else if( extension.equals( _HMO_EXTENSION ) )
    {
      support = GmlMesh2HmoConverter.SUPPORT_FLOW_RESISTANCE_CLASSES;
    }
    
    return support;
  }
}