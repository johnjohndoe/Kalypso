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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
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
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
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
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RoughnessStyleUpdater implements IGmlWorkspaceListener
{
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
  public void init( final GMLWorkspace workspace )
  {
    // nothing to do
  }

  public void exportSLD() throws IOException, SAXException, CoreException {
    final SzenarioDataProvider dataProvider = Kalypso1d2dProjectPlugin.getDefault().getDataProvider();
    exportSLD(dataProvider.getModel( IRoughnessClsCollection.class ));
  }
  
  public void exportSLD( final IRoughnessClsCollection collection ) throws IOException, SAXException, CoreException
  {
    final StyledLayerDescriptor descriptor = createSLD( collection );
    final ByteArrayInputStream stream = new ByteArrayInputStream(descriptor.exportAsXML().getBytes( "UTF-8" ));
    final Document doc = XMLTools.parse( stream );
    final Source source = new DOMSource( doc );

    final SetContentHelper helper = new SetContentHelper()
    {
      @Override
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        try
        {
          final StreamResult result = new StreamResult( writer );
          final TransformerFactory factory = TransformerFactory.newInstance();

          // Comment from Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
          // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
          try
          {
            factory.setAttribute( "indent-number", new Integer( 4 ) );
          }
          catch( final IllegalArgumentException e )
          {
          }

          final Transformer transformer = factory.newTransformer();
          transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
          transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

          // transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
          // //$NON-NLS-1$
          // //$NON-NLS-2$
          // transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
          // transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml"); //$NON-NLS-1$ //$NON-NLS-2$

          transformer.transform( source, result );
        }
        finally
        {
          IOUtils.closeQuietly( writer );
        }
      }
    };
    final IProject project = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext().getCurrentProject().getProject();
    final IFile findMember = (IFile) project.findMember( ".metadata/roughness.sld" );
    helper.setFileContents( findMember, false, true, new NullProgressMonitor() );
  }

  private StyledLayerDescriptor createSLD( final IRoughnessClsCollection collection )
  {
    final String layerName = "deegree style definition";
    final String styleName = "Roughness style";
    final String styleTitle = "Roughness style";
    final String defaultRuleName = "undefinierter Stil";
    final String defaultRuleLiteral = "_DEFAULT_STYLE_"; //$NON-NLS-1$
    final Color defaultRuleFillColor = new Color( Integer.parseInt( "aaaaaa", 16 ) ); //$NON-NLS-1$
    final Color defaultRuleStrokeColor = new Color( Integer.parseInt( "777777", 16 ) ); //$NON-NLS-1$

    final FeatureTypeStyle style = new FeatureTypeStyle_Impl();

    // adding rules for every roughness class
    for( final IRoughnessCls roughnessCls : collection )
    {
      final RGB rgb = roughnessCls.getColorStyle();
      Color color = null;
      if( rgb == null )
        color = Color.WHITE;
      else
        color = new Color( rgb.red, rgb.green, rgb.blue );
      final Stroke stroke = StyleFactory.createStroke( Color.BLACK, 1.0, 0.5 );
      final Fill fill = StyleFactory.createFill( color, 0.5 );
      final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, new PropertyName( "polygonProperty", null ) );
      final Rule rule = StyleFactory.createRule( newSymbolizer, 0.0, 1.0E15 );
      final String ruleName = roughnessCls.getName();
      final Operation operation = new PropertyIsLikeOperation( new PropertyName( "roughnessStyle", null ), new Literal( ruleName ), '*', '$', '/' );
      final Filter filter = new ComplexFilter( operation );
      rule.setName( ruleName );
      rule.setTitle( ruleName );
      rule.setAbstract( ruleName );
      rule.setFilter( filter );
      style.addRule( rule );
    }
    // adding default rule
    final Stroke stroke = StyleFactory.createStroke( defaultRuleStrokeColor, 1.0, 0.5 );
    final Fill fill = StyleFactory.createFill( defaultRuleFillColor, 0.5 );
    final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, new PropertyName( "polygonProperty", null ) );
    final Rule rule = StyleFactory.createRule( newSymbolizer, 0.0, 1.0E15 );
    final Operation operation = new PropertyIsLikeOperation( new PropertyName( "roughnessStyle", null ), new Literal( defaultRuleLiteral ), '*', '$', '/' );
    final Filter filter = new ComplexFilter( operation );
    rule.setName( defaultRuleName );
    rule.setTitle( defaultRuleName );
    rule.setAbstract( defaultRuleName );
    rule.setFilter( filter );
    style.addRule( rule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final Style[] styles = new Style[] { new UserStyle_Impl( styleName, styleTitle, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( layerName, null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
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
          final Feature changedFeatures[] = ((FeaturesChangedModellEvent) modellEvent).getFeatures();
          if( changedFeatures == null )
          {
            return;
          }
          else
          {
            for( final Feature feature : changedFeatures )
            {
              if( feature.getFeatureType().getQName().equals( m_roughnessCls ) )
              {
                final IProject project = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext().getCurrentProject().getProject();
                final IFile findMember = (IFile) project.findMember( ".metadata/roughness.sld" );
                final IUrlResolver2 resolver = new IUrlResolver2()
                {
                  public URL resolveURL( String href ) throws MalformedURLException
                  {
                    return UrlResolverSingleton.resolveUrl( findMember.getRawLocationURI().toURL(), href );
                  }
                };

                final FeatureList features = (FeatureList) feature.getParent().getProperty( feature.getParentRelation() );
                // Comment from Dejan: method SLDFactory.createSLD( file )
                // doesn't work here: cannot read UTF-8 properly
                final StyledLayerDescriptor descriptor = SLDFactory.createSLD( resolver, new FileReader( new File( findMember.getRawLocation().toOSString() ) ) );

                for( final NamedLayer layer : descriptor.getNamedLayers() )
                {
                  for( final Style style : layer.getStyles() )
                  {
                    for( final FeatureTypeStyle featureTypeStyle : ((UserStyle) style).getFeatureTypeStyles() )
                    {
                      final Rule[] rules = new Rule[features.size() + 1];
                      for( int i = 0; i < features.size(); i++ )
                      {
                        final Feature memberFeature = (Feature) features.get( i );
                        final RGB rgb = (RGB) memberFeature.getProperty( m_colorStyle );
                        Color color = null;
                        if( rgb == null )
                          color = Color.WHITE;
                        else
                          color = new Color( rgb.red, rgb.green, rgb.blue );
                        final Stroke stroke = StyleFactory.createStroke( Color.BLACK, 1.0, 0.5 );
                        final Fill fill = StyleFactory.createFill( color, 0.5 );
                        final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, new PropertyName( "polygonProperty", null ) );
                        final Rule newRule = StyleFactory.createRule( newSymbolizer, 0.0, 1.0E15 );
                        final String ruleName = ((ArrayList<String>) memberFeature.getProperty( m_name )).get( 0 );
                        final Operation operation = new PropertyIsLikeOperation( new PropertyName( "roughnessStyle" ), new Literal( ruleName ), '*', '$', '/' );
                        final Filter filter = new ComplexFilter( operation );
                        newRule.setName( ruleName );
                        newRule.setTitle( ruleName );
                        newRule.setAbstract( ruleName );
                        newRule.setFilter( filter );
                        rules[i] = newRule;
                      }
                      // adding default rule
                      final Color fillColor = new Color( Integer.parseInt( "aaaaaa", 16 ) );
                      final Color strokeColor = new Color( Integer.parseInt( "777777", 16 ) );
                      final String ruleName = "undefinierter Stil";
                      final String literal = "_DEFAULT_STYLE_";
                      final Stroke stroke = StyleFactory.createStroke( strokeColor, 1.0, 0.5 );
                      final Fill fill = StyleFactory.createFill( fillColor, 0.5 );
                      final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, new PropertyName( "polygonProperty", null ) );
                      final Rule newRule = StyleFactory.createRule( newSymbolizer, 0.0, 1.0E15 );
                      final Operation operation = new PropertyIsLikeOperation( new PropertyName( "roughnessStyle" ), new Literal( literal ), '*', '$', '/' );
                      final Filter filter = new ComplexFilter( operation );
                      newRule.setName( ruleName );
                      newRule.setTitle( ruleName );
                      newRule.setAbstract( ruleName );
                      newRule.setFilter( filter );
                      rules[rules.length - 1] = newRule;

                      featureTypeStyle.setRules( rules );
                    }
                  }
                }
                final Document doc = XMLTools.parse( new StringReader( descriptor.exportAsXML() ) );
                final Source source = new DOMSource( doc );

                final SetContentHelper helper = new SetContentHelper()
                {
                  @Override
                  protected void write( final OutputStreamWriter writer ) throws Throwable
                  {
                    try
                    {
                      final StreamResult result = new StreamResult( writer );
                      final TransformerFactory factory = TransformerFactory.newInstance();

                      // Comment from Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
                      // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
                      try
                      {
                        factory.setAttribute( "indent-number", new Integer( 4 ) );
                      }
                      catch( final IllegalArgumentException e )
                      {
                      }

                      final Transformer transformer = factory.newTransformer();
                      transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
                      transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

                      // transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
                      // //$NON-NLS-1$
                      // //$NON-NLS-2$
                      // transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
                      // transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml"); //$NON-NLS-1$ //$NON-NLS-2$

                      transformer.transform( source, result );
                    }
                    finally
                    {
                      IOUtils.closeQuietly( writer );
                    }
                  }
                };
                helper.setFileContents( findMember, false, true, new NullProgressMonitor() );
              }
            }
          }
        }
      }
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }
}
