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

package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author hübsch
 */
public class BodenartWriter extends AbstractCoreFileWriter
{
  private final GMLWorkspace m_parameterWorkspace;

  public BodenartWriter( final GMLWorkspace parameterWorkspace, final Logger logger )
  {
    super( logger );

    m_parameterWorkspace = parameterWorkspace;
  }

  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    final Feature rootFeature = m_parameterWorkspace.getRootFeature();
    final List< ? > list = (List< ? >) rootFeature.getProperty( NaModelConstants.PARA_SOIL_LAYER_MEMBER );

    writer.append( Messages.getString( "org.kalypso.convert.namodel.manager.BodenartManager.5" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    writer.append( "BODART_ID ArtKap.  WP     FK     BFMAX     Kf   BF0\n" ); //$NON-NLS-1$
    writer.append( "                [mm/dm] [mm/dm] [mm/dm]  [mm/d] [-]\n" ); //$NON-NLS-1$
    final List<String> names = new ArrayList<String>();

    final Iterator< ? > iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature bodenartFE = (Feature) iter.next();
      // TODO: nur die schreiben, die auch in Bodentyp verwendet werden.
      writeFeature( writer, bodenartFE );
      names.add( bodenartFE.getName() );
    }

    /*
     * needed for suds, fixed values temporarily... mulde kap 1.0 2.0 99.0 8640.0 0.01 rein kap 11.5 28.5 38.5 864.0
     * 0.50 filter kap 4.5 36.0 42.0 3110.0 0.50 base kap 4.5 19.0 51.0 3110.0 0.50 GR-stau kap 1.0 2.0 99.0 8640.0 0.01
     * Substr kap 4.5 19.0 42.0 350.0 0.25 Drain kap 4.5 36.0 42.0 4000.0 0.25
     */
    if( !names.contains( "mulde" ) ) //$NON-NLS-1$
      writer.append( "mulde kap 1.0 2.0 99.0 8640.0 0.01\n" ); //$NON-NLS-1$
    if( !names.contains( "rein" ) ) //$NON-NLS-1$
      writer.append( "rein kap 11.5 28.5 38.5 864.0 0.50\n" ); //$NON-NLS-1$
    if( !names.contains( "filter" ) ) //$NON-NLS-1$
      writer.append( "filter kap 4.5 36.0 42.0 3110.0 0.50\n" ); //$NON-NLS-1$
    if( !names.contains( "base" ) ) //$NON-NLS-1$
      writer.append( "base kap 4.5 19.0 51.0 3110.0 0.50\n" ); //$NON-NLS-1$
    if( !names.contains( "GR-stau" ) ) //$NON-NLS-1$
      writer.append( "GR-stau kap 1.0 2.0 99.0 8640.0 0.01\n" ); //$NON-NLS-1$
    if( !names.contains( "Substr" ) ) //$NON-NLS-1$
      writer.append( "Substr kap 4.5 19.0 42.0 350.0 0.25\n" ); //$NON-NLS-1$
    if( !names.contains( "Drain" ) ) //$NON-NLS-1$
      writer.append( "Drain kap 4.5 36.0 42.0 4000.0 0.25\n" ); //$NON-NLS-1$
  }

  private void writeFeature( final PrintWriter writer, final Feature feature ) throws Exception
  {
    // (name,*)_(typkap,*)_(typwp,*)_(typfk,*)_(typbfm,*)_(typkf,*)_(typbf0,*)
    writer.append( FortranFormatHelper.printf( feature.getName(), "*" ) + " kap " //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typwp" ), "*" ) + " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typfk" ), "*" ) + " " //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typbfm" ), "*" ) + " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typkf" ), "*" ) + " " //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typbf0" ), "*" ) + "\n" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
  }
}
