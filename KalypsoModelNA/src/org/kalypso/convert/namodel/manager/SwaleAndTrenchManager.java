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

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author huebsch
 */
public class SwaleAndTrenchManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  public SwaleAndTrenchManager( final NAConfiguration conf ) throws IOException
  {
    super( conf.getSwaleAndTrenchFormatURL() );
    m_conf = conf;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( final LineNumberReader reader ) throws Exception
  {
    // added to remove yellow things
    reader.getLineNumber();
    // TODO: code!!!
    return null;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace workspace ) throws Exception
  {
    final Feature rootFeature = workspace.getRootFeature();
    final Feature col = (Feature) rootFeature.getProperty( NaModelConstants.MRS_COLLECTION_MEMBER_PROP );
    if( col == null )
      return;
    final List< ? > list = (List< ? >) col.getProperty( NaModelConstants.MRS_MEMBER_PROP );
    final Iterator< ? > iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature swaleTrenchFE = (Feature) iter.next();
      writeFeature( asciiBuffer, workspace, swaleTrenchFE );
    }
  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final GMLWorkspace workSpace, final Feature feature ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();
    // Line 5
    final GM_Curve sTGeomProp = (GM_Curve) feature.getProperty( NaModelConstants.MRS_GEOM_PROP );
    final NaModell naModel = (NaModell) workSpace.getRootFeature();
    final IFeatureBindingCollection<Catchment> catchmentList = naModel.getCatchments();
    final Iterator<Catchment> catchmentIter = catchmentList.iterator();
    int catchmentAsciiID = 0;
    while( catchmentIter.hasNext() & catchmentAsciiID == 0 )
    {
      final Catchment catchmentFE = catchmentIter.next();
      final GM_Object tGGeomProp = (GM_Object) catchmentFE.getProperty( NaModelConstants.CATCHMENT_GEOM_PROP );
      if( tGGeomProp.contains( sTGeomProp.getStartPoint() ) )
      {
        catchmentAsciiID = idManager.getAsciiID( catchmentFE );
        asciiBuffer.getSwaleTrenchBuffer().append( catchmentAsciiID + "\n" ); //$NON-NLS-1$
      }
    }

    // Line 6
    // (area,*)_(nutzung,*)_(boden,*)_(maxPerk,*)_(InflowGW,*)
    final Double width = (Double) feature.getProperty( NaModelConstants.MRS_WIDTH_PROP );
    final double length = sTGeomProp.getLength();
    final double area = width.doubleValue() * length;
    asciiBuffer.getSwaleTrenchBuffer().append( area );
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "nutzung" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "boden" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "maxPerk" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "InflowGW" ), "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    // Line 7
    // (diameterPipe,*)_(kfPipe,*)_(drainPipeSlope,*)_(roughnessPipe,*)_(widthTrench,*)_(dischargeNode,*)
    asciiBuffer.getSwaleTrenchBuffer().append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "diameterPipe" ), "*" ) ); //$NON-NLS-1$//$NON-NLS-2$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "kfPipe" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "drainPipeSlope" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "roughnessPipe" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "widthTrench" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final IRelationType rt = (IRelationType) feature.getFeatureType().getProperty( NaModelConstants.LINK_MRS_DISCHARGE_NODE_PROP );
    final Feature nodeFeSTDischarge = workSpace.resolveLink( feature, rt );
    // ist der link nicht gesetzt wird der Defaultwert Knotennummer 0 gesetzt, im model entspircht das dem Quellknoten
    // des Teilgebietes.
    if( nodeFeSTDischarge != null )
      asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( Integer.toString( idManager.getAsciiID( nodeFeSTDischarge ) ), "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    else
      asciiBuffer.getSwaleTrenchBuffer().append( " " + FortranFormatHelper.printf( Integer.toString( 0 ), "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    asciiBuffer.getSwaleTrenchBuffer().append( "# ende MR \n " ); //$NON-NLS-1$

  }

  /**
   * @see org.kalypso.convert.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  protected String mapID( final int id, final IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }
}