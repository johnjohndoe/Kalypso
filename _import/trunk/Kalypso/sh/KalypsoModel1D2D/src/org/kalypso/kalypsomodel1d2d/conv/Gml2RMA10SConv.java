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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.Formatter;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Provides algorithm to convert from 1d2d discretisation model to bce2d model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Gml2RMA10SConv
{
  private final LinkedHashMap<String, String> lines_AR = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> lines_FP = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> lines_FE = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> lines_RK = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> lines_CS = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> m_idProvider = new LinkedHashMap<String, String>();
  
  private int m_nextID = 1;
  
  private double m_offsetX;

  private double m_offsetY;

  private double m_offsetZ;

  private IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private URL m_outputURL;

  //  
  //  
  //  
  //
  // // private IModelElementIDProvider idProvider;
  //
  // private IRMA10SModelElementHandler handler;
  //
  // private GMLWorkspace m_workspace;
  //
  // public static boolean verboseMode = true;
  //
  // /**
  // * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelWriter#write(java.io.OutputStream)
  // */
  // public void write( OutputStream outputStream ) throws IllegalStateException, IOException
  // {
  // Assert.throwIAEOnNullParam( outputStream, "outputStream" );
  // this.write( new OutputStreamWriter( outputStream ) );
  // }
  //
  // /**
  // * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelWriter#write(java.io.OutputStreamWriter)
  // */
  // public void write( OutputStreamWriter outputStreamWriter ) throws IllegalStateException, IOException
  // {
  // Assert.throwIAEOnNullParam( outputStreamWriter, "outputStreamWriter" );
  // Object property = m_workspace.getRootFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
  //
  // }
  //
  // /**
  // * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#parse(java.io.InputStreamReader)
  // */
  // public void parse( InputStreamReader inputStreamReader ) throws IllegalStateException, IOException
  // {
  // Assert.throwIAEOnNullParam( inputStreamReader, "inputStreamReader" );
  // LineNumberReader reader = new LineNumberReader( inputStreamReader );
  // try
  // {
  // char char0, char1;
  // int length;
  //
  // // signal parsing start
  // handler.start();
  //
  // for( String line = reader.readLine(); line != null; line = reader.readLine() )
  // {
  // // if(verboseMode) System.out.println(line);
  // length = line.length();
  // if( line.length() < 2 )
  // {
  // continue;
  // }
  //
  // char0 = line.charAt( 0 );
  // char1 = line.charAt( 1 );
  //
  // if( char0 == 'F' && char1 == 'P' )
  // {
  // interpreteNodeLine( length, line, handler );
  //
  // }
  // else if( char0 == 'F' && char1 == 'E' )
  // {
  // // LineID, ID
  // interpreteElementLine( length, line, handler );
  // }
  // else if( char0 == 'A' && char1 == 'R' )
  // {
  // // edge LINEID, ID, node1, node2, ellinks, elrechts
  // interpreteArcLine( length, line, handler );
  // }
  // else if( char0 == 'R' && char1 == 'K' )
  // {
  //
  // }
  // else
  // {
  // if( verboseMode )
  // System.out.println( "Unsupported section:" + line );
  // }
  // }
  //
  // // signal parsing stop
  // handler.end();
  // }
  // catch( IOException e )
  // {
  // throw new IOException();
  // }
  //
  // }
  //
  // /**
  // * @see
  // org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
  // */
  // public void setModelElementIDProvider( IModelElementIDProvider idProvider ) throws IllegalArgumentException
  // {
  // // this.idProvider=idProvider;
  // }
  //
  // /**
  // * @see
  // org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
  // */
  // public void setRMA10SModelElementHandler( IRMA10SModelElementHandler handler ) throws IllegalArgumentException
  // {
  // this.handler = handler;
  // }
  //
  private int getID( String gmlNodeID )
  {
    if(m_idProvider.containsKey( gmlNodeID ))
      return Integer.parseInt( m_idProvider.get( gmlNodeID ) );
    else
    {
      m_idProvider.put( gmlNodeID, Integer.toString( m_nextID ) );
      return m_nextID++;
    }
  }

  public Gml2RMA10SConv( IFEDiscretisationModel1d2d sourceModel, URL rma10sOutputURL, IPositionProvider positionProvider, IModelElementIDProvider idProvider )
  {
    this.m_discretisationModel1d2d = sourceModel;
    this.m_outputURL = rma10sOutputURL;
    GM_Point point = positionProvider.getGMPoint( 0.0, 0.0, 0.0 );
    m_offsetX = -point.getX();
    m_offsetY = -point.getY();
    m_offsetZ = -point.getZ();
    // NOT USED: idProvider;
  }

  public void toRMA10sModel( ) throws IllegalStateException, IOException
  {
    // IRMA10SModelElementHandler handler = new DiscretisationModel1d2dHandler( sourceModel, positionProvider,
    // idProvider );
    // writer.setModelElementIDProvider( idProvider );
    // writer.setRMA10SModelElementHandler( handler );

    IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = m_discretisationModel1d2d.getComplexElements();
    IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();
    IFeatureWrapperCollection<IFE1D2DNode> nodes = m_discretisationModel1d2d.getNodes();
    IFeatureWrapperCollection<IFE1D2DEdge> edges = m_discretisationModel1d2d.getEdges();

    Iterator<IFE1D2DComplexElement> complexElementsIterator = complexElements.iterator();
    while( complexElementsIterator.hasNext() )
    {
      IFE1D2DComplexElement complexElement = complexElementsIterator.next();
      // TODO process it
    }

    Iterator<IFE1D2DElement> elementsIterator = elements.iterator();
    while( elementsIterator.hasNext() )
    {
      IFE1D2DElement element = elementsIterator.next();
      // TODO process it
    }

    Iterator<IFE1D2DNode> nodesIterator = nodes.iterator();
    while( nodesIterator.hasNext() )
    {
      IFE1D2DNode node = nodesIterator.next();
      String nodeGmlID = node.getGmlID();
      GM_Point point = correctPosition( node.getPoint() );
      StringBuilder builder = new StringBuilder();
      Formatter formatter = new Formatter( builder );
      formatter.format( Locale.US, "FP%10d%20.7f%20.7f%20.7f", getID(nodeGmlID), point.getX(), point.getY(), point.getZ() );
      lines_FP.put( nodeGmlID, builder.toString() );
      builder.setLength( 0 );
      formatter.format( Locale.US, "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f", getID(nodeGmlID), 10.0, 2.0, 2.0, 0.0, 0.0, 0.0 );
      lines_CS.put( nodeGmlID, builder.toString() );
    }

    Iterator<IFE1D2DEdge> edgesIterator = edges.iterator();
    while( edgesIterator.hasNext() )
    {
      IFE1D2DEdge edge = edgesIterator.next();
      edge.getNodes();
      // TODO prosess it
    }
  }

  private GM_Point correctPosition( GM_Point point )
  {
    return GeometryFactory.createGM_Point( point.getX() + m_offsetX, point.getY() + m_offsetY, point.getZ() + m_offsetZ, point.getCoordinateSystem() );
  }

  public void write( ) throws IOException
  {
    PrintStream stream = new PrintStream(new File(m_outputURL.getPath()));
    Iterator<String> iterator;
    iterator = lines_FP.keySet().iterator();
    while( iterator.hasNext() )
      stream.println( lines_FP.get( iterator.next() ) );
    iterator = lines_CS.keySet().iterator();
    while( iterator.hasNext() )
      stream.println( lines_CS.get( iterator.next() ) );
    stream.flush();
    stream.close();
  }

  // private static final void interpreteNodeLine( final int length, final String line, final IRMA10SModelElementHandler
  // handler )
  // {
  // if( length == 72 )
  // {
  // if( verboseMode )
  // System.out.println( line + "[" + line.substring( 3 - 1, 12 ) + "]" );
  // int id = Integer.parseInt( line.substring( 3 - 1, 12 ).trim() );
  // double easting = Double.parseDouble( line.substring( 13 - 1, 32 ).trim() );
  // double northing = Double.parseDouble( line.substring( 33 - 1, 52 ).trim() );
  //
  // double elevation = Double.parseDouble( line.substring( 53 - 1, 72 ).trim() );
  //
  // handler.handleNode( line, id, easting, northing, elevation );
  // }
  // else
  // {
  // handler.handlerError( line, EReadError.LINE_TOO_SHORT );
  // }
  // }
  //
  // private static final void interpreteArcLine( final int length, final String line, final IRMA10SModelElementHandler
  // handler )
  // {
  // if( length == 52 )
  // {// no middle node
  // try
  // {
  // int id = Integer.parseInt( line.substring( 3 - 1, 12 ).toString().trim() );
  // int node1ID = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int node2ID = Integer.parseInt( line.substring( 23 - 1, 32 ).trim() );
  // int elementLeftID = Integer.parseInt( line.substring( 33 - 1, 42 ).trim() );
  // int elementRightID = Integer.parseInt( line.substring( 43 - 1, 52 ).trim() );
  // int middleNodeID = -1;// Integer.parseInt( line.substring( 43-1,52-1 ));
  // handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
  // }
  // catch( Throwable th )
  // {
  // th.printStackTrace();
  // handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  // }
  // }
  // else if( length == 62 )
  // {// no middle node
  // try
  // {
  // int id = Integer.parseInt( line.substring( 3 - 1, 12 ).trim() );
  // int node1ID = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int node2ID = Integer.parseInt( line.substring( 23 - 1, 32 ).trim() );
  // int elementLeftID = Integer.parseInt( line.substring( 33 - 1, 42 ).trim() );
  // int elementRightID = Integer.parseInt( line.substring( 43 - 1, 52 ).trim() );
  // int middleNodeID = Integer.parseInt( line.substring( 43 - 1, 52 ).trim() );
  // handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
  // }
  // catch( Throwable th )
  // {
  // handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  // }
  // }
  // else
  // {
  // handler.handlerError( line, EReadError.LINE_TOO_SHORT );
  // }
  // }
  //
  // private static final void interpreteElementLine( final int length, final String line, final
  // IRMA10SModelElementHandler handler )
  // {
  // if( length == 22 )
  // {
  // int id = Integer.parseInt( line.substring( 3 - 1, 12 ).trim() );
  // int currentRougthnessClassID = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int previousRoughnessClassID = -1;// Integer.parseInt( line.substring( 33-1, 42 ));
  // int eleminationNumber = -1;// Integer.parseInt( line.substring( 43-1, 52 ));
  // handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
  // }
  // else if( length == 32 )
  // {
  // int id = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int currentRougthnessClassID = Integer.parseInt( line.substring( 23 - 1, 32 ).trim() );
  // int previousRoughnessClassID = -1;// Integer.parseInt( line.substring( 33-1, 42 ));
  // int eleminationNumber = -1;// Integer.parseInt( line.substring( 43-1, 52 ));
  // handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
  // }
  // else if( length == 42 )
  // {
  // int id = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int currentRougthnessClassID = Integer.parseInt( line.substring( 23 - 1, 32 ).trim() );
  // int previousRoughnessClassID = Integer.parseInt( line.substring( 33 - 1, 42 ).trim() );
  // int eleminationNumber = -1;// Integer.parseInt( line.substring( 43-1, 52 ));
  // handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
  // }
  // else if( length == 52 )
  //
  // {
  // int id = Integer.parseInt( line.substring( 13 - 1, 22 ).trim() );
  // int currentRougthnessClassID = Integer.parseInt( line.substring( 23 - 1, 32 ).trim() );
  // int previousRoughnessClassID = Integer.parseInt( line.substring( 33 - 1, 42 ).trim() );
  // int eleminationNumber = Integer.parseInt( line.substring( 43 - 1, 52 ).trim() );
  // handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
  // }
  // else
  // {
  // handler.handlerError( line, EReadError.LINE_TOO_SHORT );
  // }
  // }

}
