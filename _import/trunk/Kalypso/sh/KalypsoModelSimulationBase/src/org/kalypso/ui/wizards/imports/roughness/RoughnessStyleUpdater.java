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
package org.kalypso.ui.wizards.imports.roughness;

import java.awt.Color;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.w3c.dom.Document;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RoughnessStyleUpdater implements IGmlWorkspaceListener
{
  private GMLWorkspace m_workspace;

  private final static QName m_roughnessCls = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessCls" );

  private final static QName m_name = new QName( NS.GML3, "name" );

  private final static QName m_colorStyle = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "colorStyle" );

  public RoughnessStyleUpdater( )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#getQNames()
   */
  public QName[] getQNames( )
  {
    return new QName[] {};
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#init(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void init( GMLWorkspace workspace )
  {
    this.m_workspace = workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    try
    {
      if( modellEvent == null )
      {
        return;
      }
      else if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
      {
        if( modellEvent instanceof FeaturesChangedModellEvent )
        {
          Feature changedFeatures[] = ((FeaturesChangedModellEvent) modellEvent).getFeatures();
          if( changedFeatures == null )
          {
            return;
          }
          else
          {
            for( Feature feature : changedFeatures )
            {
              if( feature.getFeatureType().getQName().equals( m_roughnessCls ) )
              {
                final URL url = new URL( "file:/"
                    + ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_workspace.getContext().getPath().replaceFirst( "/resource/", "/" ) ).removeLastSegments( 1 ).append( "/roughness.sld" ) );
                final IUrlResolver2 resolver = new IUrlResolver2()
                {
                  public URL resolveURL( String href ) throws MalformedURLException
                  {
                    return UrlResolverSingleton.resolveUrl( url, href );
                  }

                };
                File file = new File( url.getPath() );
                StyledLayerDescriptor descriptor = SLDFactory.createSLD( resolver, new FileReader( file ) );
                for( NamedLayer layer : descriptor.getNamedLayers() )
                {
                  for( Style style : layer.getStyles() )
                  {
                    for( FeatureTypeStyle featureTypeStyle : ((UserStyle) style).getFeatureTypeStyles() )
                    {
                      boolean ruleFound = false;
                      for( Rule rule : featureTypeStyle.getRules() )
                      {
                        if( rule.getFilter() != null && rule.getFilter().evaluate( feature ) )
                        {
                          ruleFound = true;
                          for( Symbolizer symbolizer : rule.getSymbolizers() )
                          {
                            if( symbolizer instanceof PolygonSymbolizer )
                            {
                              RGB rgb = (RGB) feature.getProperty( m_colorStyle );
                              ((PolygonSymbolizer) symbolizer).getFill().setFill( new Color( rgb.red, rgb.green, rgb.blue ) );
                            }
                            break;
                          }
                          break;
                        }
                      }
                      if( !ruleFound )
                      {
                        final RGB rgb = (RGB) feature.getProperty( m_colorStyle );
                        final Color color = new Color( rgb.red, rgb.green, rgb.blue );
                        final Stroke stroke = StyleFactory.createStroke( Color.BLACK, 1.0, 0.5 );
                        final Fill fill = StyleFactory.createFill( color, 0.5 );
                        final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, "polygonProperty" );
                        final Rule newRule = StyleFactory.createRule( newSymbolizer, 0.0, 1.0E15 );
                        final Operation operation = new PropertyIsLikeOperation( new PropertyName( "roughnessStyle" ), new Literal( ((ArrayList<String>) feature.getProperty( m_name )).get( 0 ) ), '*', '$', '/' );
                        final Filter filter = new ComplexFilter( operation );
                        newRule.setFilter( filter );
                        featureTypeStyle.addRule( newRule );
                      }
                    }
                  }
                }
                final Document doc = XMLTools.parse( new StringReader( descriptor.exportAsXML() ) );
                final Source source = new DOMSource( doc );
                OutputStreamWriter os = null;
                try
                {
                  os = new FileWriter( file );
                  StreamResult result = new StreamResult( os );
                  TransformerFactory factory = TransformerFactory.newInstance();

                  // Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
                  // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
                  try
                  {
                    factory.setAttribute( "indent-number", new Integer( 4 ) );
                  }
                  catch( IllegalArgumentException e )
                  {
                  }

                  Transformer transformer = factory.newTransformer();
                  transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
                  transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

                  // transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$
                  // //$NON-NLS-2$
                  // transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
                  // transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml"); //$NON-NLS-1$ //$NON-NLS-2$

                  transformer.transform( source, result );
                }
                finally
                {
                  IOUtils.closeQuietly( os );
                }
                ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
              }
            }
          }
        }
      }
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }
}
