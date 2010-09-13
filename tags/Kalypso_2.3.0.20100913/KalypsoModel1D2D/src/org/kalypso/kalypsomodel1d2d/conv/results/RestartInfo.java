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
package org.kalypso.kalypsomodel1d2d.conv.results;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RestartInfo extends AbstractFeatureBinder implements IRestartInfo
{
  public RestartInfo( final Feature featureToBind )
  {
    super( featureToBind, IRestartInfo.QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#getCalculationUnitID()
   */
  @Override
  public String getCalculationUnitID( )
  {
    return (String) getFeature().getProperty( IRestartInfo.QNAME_PROP_CALC_UNIT_ID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#getStepResultMetaID()
   */
  @Override
  public String getStepResultMetaID( )
  {
    return (String) getFeature().getProperty( IRestartInfo.QNAME_PROP_STEP_RESULT_ID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#getRestartFilePath()
   */
  @Override
  public IPath getRestartFilePath( )
  {
    final String path = (String) getFeature().getProperty( IRestartInfo.QNAME_PROP_RESULT_FILE_PATH );
    if( path == null )
      return null;
    return Path.fromPortableString( path );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#setCalculationUnitID(java.lang.String)
   */
  @Override
  public void setCalculationUnitID( final String gmlID )
  {
    getFeature().setProperty( IRestartInfo.QNAME_PROP_CALC_UNIT_ID, gmlID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#setStepResultMetaID(java.lang.String)
   */
  @Override
  public void setStepResultMetaID( final String gmlID )
  {
    getFeature().setProperty( IRestartInfo.QNAME_PROP_STEP_RESULT_ID, gmlID );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo#setRestartFilePath(java.lang.String)
   */
  @Override
  public void setRestartFilePath( final String filePath )
  {
    getFeature().setProperty( IRestartInfo.QNAME_PROP_RESULT_FILE_PATH, filePath );
  }

}
