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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides algorithm to convert between a bce2d model and
 * a 1d2d discretisation model
 * 
 * @author Patrice Congo
 */
public class RMA10S2GmlConv implements IRMA10SModelReader
{
  
//  private IModelElementIDProvider idProvider;
  
  private IRMA10SModelElementHandler handler;
  

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#parse(java.io.InputStream)
   */
  public void parse( 
                InputStream inputStream ) 
                throws IllegalStateException, IOException
  {
    Assert.throwIAEOnNullParam( inputStream, "inputStream" );
    this.parse( new InputStreamReader(inputStream) );
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#parse(java.io.InputStreamReader)
   */
  public void parse( 
                        InputStreamReader inputStreamReader ) 
                        throws IllegalStateException, IOException
  {
      Assert.throwIAEOnNullParam( 
                          inputStreamReader, "inputStreamReader" );
      LineNumberReader reader= 
          new LineNumberReader(inputStreamReader);
      try
      {
        char char0, char1;
        int length;
        
        //signal parsing start
        handler.start();
        
        for(
            String line=reader.readLine();
            line!=null;
            line=reader.readLine())
        {
//          System.out.println(line);
          length=line.length();
          if(line.length()<2)
          {
            continue;
          }
          
          char0=line.charAt( 0 );
          char1=line.charAt( 1 );
          
          if(char0=='F' && char1=='P')
          {
            interpreteNodeLine( length, line, handler );

          }
          else if(char0=='F' && char1=='E')
          {
            //LineID, ID
            interpreteElementLine( length, line, handler );
          }
          else if(char0=='A' && char1=='R')
          {
            //edge LINEID, ID, node1, node2, ellinks, elrechts
            interpreteArcLine( length, line, handler );
          }
          else if(char0=='R' && char1=='K')
          {
            
          }
          else
          {
            System.out.println("Unsupported section:"+line);
          }
        }
        
        //signal parsing stop
        handler.end();
    }
    catch (IOException e) 
    {
      throw new IOException();
    }
      
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( 
                        IModelElementIDProvider idProvider ) 
                        throws IllegalArgumentException
  {
//    this.idProvider=idProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
   */
  public void setRMA10SModelElementHandler( 
                        IRMA10SModelElementHandler handler ) 
                        throws IllegalArgumentException
  {
    this.handler=handler;
  }

  static public  void toDiscretisationModel(
                                  InputStream rma10sModelInput, 
                                  IFEDiscretisationModel1d2d targetModel,
                                  CS_CoordinateSystem coordinateSystem,
                                  IModelElementIDProvider idProvider) throws IllegalStateException, IOException
  {
    IRMA10SModelReader reader= new RMA10S2GmlConv();
    
    IRMA10SModelElementHandler handler= 
                 new DiscretisationModel1d2dHandler(
                                 targetModel, coordinateSystem, idProvider);
    reader.setModelElementIDProvider( idProvider );
    reader.setRMA10SModelElementHandler( handler );
    reader.parse( rma10sModelInput );
    return;
  }
  
  private static final void interpreteNodeLine(
                                        final int length, 
                                        final String line,
                                        final IRMA10SModelElementHandler handler)
  {
    if(length==72)
    {
      System.out.println(line+"["+line.substring( 3-1, 12 )+"]");
      int id = 
                Integer.parseInt( line.substring( 3-1, 12 ).trim() );
      double easting =
                Double.parseDouble( line.substring( 13-1, 32 ).trim() );
      double northing =
                Double.parseDouble( line.substring( 33-1, 52 ).trim());
      
      double elevation =
                Double.parseDouble( line.substring( 53-1, 72 ).trim());

      handler.handleNode( 
                line, 
                id, 
                easting, 
                northing, 
                elevation );
    }
    else
    {
      handler.handlerError( line, EReadError.LINE_TOO_SHORT );
    }
  }
  
  private static final void interpreteArcLine(
                                final int length, 
                                final String line,
                                final IRMA10SModelElementHandler handler)
  {
     if(length==52)
     {//no middle node
       try
       {
         int id= Integer.parseInt( line.substring( 3-1,12 ).toString().trim() );
         int node1ID=Integer.parseInt( line.substring( 13-1, 22 ).trim());
         int node2ID=Integer.parseInt( line.substring( 23-1, 32 ).trim());
         int elementLeftID=Integer.parseInt( line.substring( 33-1,42  ).trim());
         int elementRightID=Integer.parseInt( line.substring( 43-1,52  ).trim());
         int middleNodeID=-1;//Integer.parseInt( line.substring( 43-1,52-1  ));
         handler.handleArc( 
                   line,id, 
                   node1ID, node2ID, 
                   elementLeftID, elementRightID, 
                   middleNodeID );
       }
       catch(Throwable th)
       {
         th.printStackTrace();
         handler.handlerError( line, EReadError.ILLEGAL_SECTION);
       }
     }
     else if(length==62)
     {//no middle node
       try
       {
         int id= Integer.parseInt( line.substring( 3-1,12).trim() );
         int node1ID=Integer.parseInt( line.substring( 13-1, 22 ).trim());
         int node2ID=Integer.parseInt( line.substring( 23-1, 32 ).trim());
         int elementLeftID=Integer.parseInt( line.substring( 33-1,42).trim());
         int elementRightID=Integer.parseInt( line.substring( 43-1,52).trim());
         int middleNodeID=Integer.parseInt( line.substring( 43-1,52).trim());
         handler.handleArc( 
                   line,id, 
                   node1ID, node2ID, 
                   elementLeftID, elementRightID, 
                   middleNodeID );
       }
       catch(Throwable th)
       {
         handler.handlerError( line, EReadError.ILLEGAL_SECTION);
       }
     }
     else
     {
       handler.handlerError( line, EReadError.LINE_TOO_SHORT);
     }
  }

  
  private static final void interpreteElementLine(
                                final int length, 
                                final String line,
                                final IRMA10SModelElementHandler handler)
  {
    if(length==22)
    {
      int id= Integer.parseInt( line.substring( 3-1,12 ).trim() );
      int currentRougthnessClassID=Integer.parseInt( 
                                      line.substring( 13-1, 22).trim());
      int previousRoughnessClassID=-1;//Integer.parseInt( line.substring( 33-1, 42 ));
      int eleminationNumber=-1;//Integer.parseInt( line.substring( 43-1, 52 ));
      handler.handleElement( 
              line, id, 
              currentRougthnessClassID, 
              previousRoughnessClassID, 
              eleminationNumber );
    }
    else if(length==32)
    {
      int id= Integer.parseInt( line.substring( 13-1,22 ).trim() );
      int currentRougthnessClassID=Integer.parseInt( line.substring( 23-1, 32).trim());
      int previousRoughnessClassID=-1;//Integer.parseInt( line.substring( 33-1, 42 ));
      int eleminationNumber=-1;//Integer.parseInt( line.substring( 43-1, 52 ));
      handler.handleElement( 
              line, id, 
              currentRougthnessClassID, 
              previousRoughnessClassID, 
              eleminationNumber );
    }
    else if(length==42)
    {
      int id= Integer.parseInt( line.substring( 13-1,22).trim() );
      int currentRougthnessClassID=Integer.parseInt( line.substring( 23-1, 32).trim());
      int previousRoughnessClassID=Integer.parseInt( line.substring( 33-1, 42 ).trim());
      int eleminationNumber=-1;//Integer.parseInt( line.substring( 43-1, 52 ));
      handler.handleElement( 
              line, id, 
              currentRougthnessClassID, 
              previousRoughnessClassID, 
              eleminationNumber );
    }
    else if(length==52)
      
    {
      int id= Integer.parseInt( line.substring( 13-1,22).trim() );
      int currentRougthnessClassID=Integer.parseInt( line.substring( 23-1, 32).trim());
      int previousRoughnessClassID=Integer.parseInt( line.substring( 33-1, 42 ).trim());
      int eleminationNumber=Integer.parseInt( line.substring( 43-1, 52 ).trim());
      handler.handleElement( 
              line, id, 
              currentRougthnessClassID, 
              previousRoughnessClassID, 
              eleminationNumber );
    }
    else
    {
      handler.handlerError( line, EReadError.LINE_TOO_SHORT );
    }
  }
  
}
