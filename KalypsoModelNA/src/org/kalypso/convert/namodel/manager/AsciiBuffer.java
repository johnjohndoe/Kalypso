/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.manager;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class AsciiBuffer
{
  private final StringBuffer m_netBuffer = new StringBuffer();

  private final StringBuffer m_catchmentBuffer = new StringBuffer();

  private final StringBuffer m_channelBuffer = new StringBuffer();

  private final StringBuffer m_rhbBuffer = new StringBuffer();

  private final StringBuffer m_hydBuffer = new StringBuffer();

  private final StringBuffer m_bodartBuffer = new StringBuffer();

  private final StringBuffer m_bodtypBuffer = new StringBuffer();

  private final StringBuffer m_snowBuffer = new StringBuffer();

  private final List<Feature> m_featuresToWrite = new ArrayList<Feature>();

  private final StringBuffer m_zftBuffer= new StringBuffer();
  private final StringBuffer m_swaleTrenchBuffer= new StringBuffer();

  public AsciiBuffer()
  {
  // nothing to do here
  }

  public void addFeatureToWrite( Feature feature )
  {
    if( !m_featuresToWrite.contains( feature ) )
      m_featuresToWrite.add( feature );
  }

  public boolean writeFeature( Feature feature )
  {
    return m_featuresToWrite.contains( feature );
  }

  public StringBuffer getNetBuffer()
  {
    return m_netBuffer;
  }

  public StringBuffer getChannelBuffer()
  {
    return m_channelBuffer;
  }

  public StringBuffer getCatchmentBuffer()
  {
    return m_catchmentBuffer;
  }

  public StringBuffer getRhbBuffer()
  {
    return m_rhbBuffer;
  }

  public StringBuffer getHydBuffer()
  {
    return m_hydBuffer;
  }

  public StringBuffer getBodartBuffer()
  {
    return m_bodartBuffer;
  }

  public StringBuffer getBodtypBuffer()
  {
    return m_bodtypBuffer;
  }

  public StringBuffer getSnowBuffer()
  {
    return m_snowBuffer;
  }

  public StringBuffer getZFTBuffer()
  {
    return m_zftBuffer;
  }

  public StringBuffer getSwaleTrenchBuffer( )
  {
    return m_swaleTrenchBuffer;
  }
}