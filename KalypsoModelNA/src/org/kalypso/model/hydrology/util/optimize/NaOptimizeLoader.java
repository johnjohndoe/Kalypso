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
package org.kalypso.model.hydrology.util.optimize;

import java.net.URL;
import java.util.Collections;
import java.util.Iterator;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.NaOptimizeData;
import org.kalypso.optimize.OptimizeJaxb;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationDataUtils;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Gernot Belger
 */
public class NaOptimizeLoader
{
  private final URL m_optimizeDataLocation;

  private final String m_optimizePath;

  private final URL m_autocalibrationLocation;

  private NaOptimizeData m_optimizeData;

  private final int m_optimizeStep;

  public NaOptimizeLoader( final ISimulationDataProvider dataProvider ) throws SimulationException
  {
    this( dataProvider, 0 );
  }

  public NaOptimizeLoader( final ISimulationDataProvider dataProvider, final int optimizeStep ) throws SimulationException
  {
    m_autocalibrationLocation = (URL) SimulationDataUtils.getInputOrNull( dataProvider, NaModelConstants.IN_OPTIMIZECONF_ID );
    m_optimizeDataLocation = SimulationDataUtils.getInputOrNull( dataProvider, NaModelConstants.IN_OPTIMIZE_ID );
    m_optimizePath = SimulationDataUtils.getInputOrDefault( dataProvider, NaModelConstants.IN_OPTIMIZE_FEATURE_PATH_ID, "." );
    m_optimizeStep = optimizeStep;
  }

  public NaOptimizeData load( final GMLWorkspace contextWorkspace, final IFeatureProviderFactory factory ) throws Exception
  {
    if( m_optimizeData == null )
      m_optimizeData = loadOptimizeData( contextWorkspace, factory );

    return m_optimizeData;
  }

  private NaOptimizeData loadOptimizeData( final GMLWorkspace contextWorkspace, final IFeatureProviderFactory factory ) throws SimulationException
  {
    try
    {
      final AutoCalibration calibration = loadCalibration();

      final NodeList optimizeNodes = loadOptimizeNodes();
      if( optimizeNodes == null )
        return new NaOptimizeData( calibration, null );

      if( optimizeNodes.getLength() == 0 )
        throw new SimulationException( String.format( "Unable to find NaOptimizeConfig for path '%s'", m_optimizePath ) );

      final Node optimizeDom = optimizeNodes.item( 0 );

      /* At the moment, we are only interested in the result-links */
      final NaOptimizeData naOptimizeData = new NaOptimizeData( calibration, optimizeDom );
      final URL context = contextWorkspace == null ? m_optimizeDataLocation : contextWorkspace.getContext();
      naOptimizeData.initNaOptimize( context, factory );
      return naOptimizeData;
    }
    catch( final SimulationException e )
    {
      throw e;
    }
    catch( final Exception e )

    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei Lesen der Optimierungskonfiguration", e );
    }
  }

  public NodeList loadOptimizeNodes( ) throws Exception
  {
    if( m_optimizeDataLocation == null )
      return null;

    final Document dom = XMLHelper.getAsDOM( m_optimizeDataLocation, true );

    final XPathFactory xpFactory = XPathFactory.newInstance();
    final javax.xml.xpath.XPath xp = xpFactory.newXPath();
    final NamespaceContext namespaceContext = new NamespaceContext()
    {
      @Override
      public String getNamespaceURI( final String prefix )
      {
        // REALLY UGLY: xpath does not know how to handle the default namespace; so we can only use an xpath with a
        // prefix; but this prefix is not defined in the gml, so it is never found.
        // We assume that the used prefix is always 'gnModell', but is meant to be the default namespace
        // TODO: we should get the prefix mapping from the input as well
        if( "gnModell".equals( prefix ) )
          return dom.getDocumentElement().getNamespaceURI();

        return dom.lookupNamespaceURI( prefix );
      }

      @Override
      public String getPrefix( final String namespaceURI )
      {
        return dom.lookupPrefix( namespaceURI );
      }

      @Override
      public Iterator< ? > getPrefixes( final String namespaceURI )
      {
        return Collections.emptyList().iterator();
      }
    };

    xp.setNamespaceContext( namespaceContext );
    return (NodeList) xp.evaluate( m_optimizePath, dom, XPathConstants.NODESET );
  }

  private AutoCalibration loadCalibration( ) throws JAXBException
  {
    if( m_autocalibrationLocation == null )
      return null;

    final Unmarshaller unmarshaller = OptimizeJaxb.JC.createUnmarshaller();
    return (AutoCalibration) unmarshaller.unmarshal( m_autocalibrationLocation );
  }

  public String getOptimizePath( )
  {
    return m_optimizePath;
  }

  public NodeList loadOptimizeNodesForMulti( ) throws SimulationException
  {
    try
    {
      final NodeList optimizeNodes = loadOptimizeNodes();
      if( optimizeNodes == null || optimizeNodes.getLength() == m_optimizeStep )
        throw new SimulationException( String.format( "Unable to find NaOptimizeConfig for path '%s'", m_optimizePath ) );

      return optimizeNodes;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei Lesen der Optimierungskonfiguration" );
    }
  }
}
