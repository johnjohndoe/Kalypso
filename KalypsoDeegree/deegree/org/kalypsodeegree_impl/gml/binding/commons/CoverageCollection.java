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
package org.kalypsodeegree_impl.gml.binding.commons;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.FileValueModelType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * @author Gernot Belger
 */
public class CoverageCollection extends FeatureWrapperCollection<ICoverage> implements ICoverageCollection
{
  /** Creates a new workspace with this collection as root feature. */
  public CoverageCollection( final URL context, final IFeatureProviderFactory providerFactory ) throws InvocationTargetException
  {
    this( FeatureFactory.createGMLWorkspace( ICoverageCollection.QNAME, context, providerFactory ).getRootFeature() );
  }

  public CoverageCollection( final Feature featureCol )
  {
    super( featureCol, ICoverage.class, CoverageCollection.QNAME_PROP_COVERAGE_MEMBER );
  }

  public static ICoverage addCoverage( final ICoverageCollection coverages, final RectifiedGridDomain domain, final String externalResource, final String mimeType )
  {
    final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();

    // file name relative to the gml
    rangeSetFile.setFileName( externalResource );
    rangeSetFile.setMimeType( mimeType );
    rangeSetFile.setFileStructure( FileValueModelType.RECORD_INTERLEAVED );

    final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
    rangeSet.setFile( rangeSetFile );

    final RectifiedGridCoverage coverage = (RectifiedGridCoverage) coverages.addNew( RectifiedGridCoverage.QNAME );

    coverage.setDescription( "Imported via Kalypso" );
    coverage.setGridDomain( domain );
    coverage.setRangeSet( rangeSet );

    return coverage;
  }
}
