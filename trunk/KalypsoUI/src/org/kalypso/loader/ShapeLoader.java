package org.kalypso.loader;

import java.io.File;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.LoaderException;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author schlienger
 *  
 */
public class ShapeLoader implements ILoader
{
  private Properties m_source;

  /**
   * @see org.kalypso.util.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#load(java.util.Properties, java.lang.Object)
   */
  public Object load( final Properties source, final Object helper ) throws LoaderException
  {
    try
    {
      final IProject project = (IProject)helper;

      final String sourceType = source.getProperty( "SERVICE", "FILE" );
      final String sourcePath = source.getProperty( "PATH", "" );
      
      File sourceFile;
      if( sourceType.equals( "FILE" ) )
        sourceFile = new File( sourcePath );
      else
      //RESOURCE
      {
        final IFile file = project.getFile( sourcePath );
        sourceFile = file.getLocation().toFile();
      }

      final String sourceSrs = source.getProperty( "SRS", "" );
      
      final ShapeFile sf = new ShapeFile( sourceFile.getAbsolutePath() );
      
      final int count = sf.getRecordNum();
      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();
      final String name = source + featureType.getName();
      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer(name, featureType,KalypsoGisPlugin.getDefault().getCoordinatesSystem());

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher Anpassung hier:

      ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();

      CS_CoordinateSystem srcCS = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( sourceSrs ) );

      for( int i = 0; i < count;i++)// TODO undo:count; i++ )
      {
          final Feature fe = sf.getFeatureByRecNo( i + 1 );       
          setCrs( fe, srcCS);
          layer.addFeature( fe );
      }

      sf.close();
      layer.optimize();

      return layer;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }

  private void setCrs( Feature fe, CS_CoordinateSystem srcCS)
  {
    final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        setCrs( (Feature)prop, srcCS);
      else if( prop != null && prop instanceof GM_Object )
      {
        ( (GM_Object_Impl)prop ).setCoordinateSystem( srcCS );
      }
    }
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#save(java.util.Properties, java.lang.Object)
   */
  public void save( final Properties source, final Object data ) throws LoaderException
  {
    // TODO: Transformation vom laden muss rueckgaengig gemacht werden !
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#setSource(java.util.Properties)
   */
  public void setSource( final Properties source )
  {
    m_source = source;
//
//    if( m_text != null && !m_text.isDisposed() )
//      m_text.setText( source );
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getSource()
   */
  public Properties getSource()
  {
    return m_source;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#createControl(java.lang.Object)
   */
  public Object createControl( final Object argument )
  {
    final Composite c = new Composite( (Composite)argument, SWT.NONE );
//    final RowLayout rowLayout = new RowLayout();
//    rowLayout.wrap = false;
//    rowLayout.pack = false;
//    rowLayout.justify = true;
//    rowLayout.type = SWT.VERTICAL;
//    rowLayout.marginLeft = 5;
//    rowLayout.marginTop = 5;
//    rowLayout.marginRight = 5;
//    rowLayout.marginBottom = 5;
//    rowLayout.spacing = 5;
//    c.setLayout( rowLayout );
//
//    m_text = new Text( c, SWT.SINGLE );
//    m_text.setText( m_source );
//    m_text.setEditable( false );
//
//    m_button = new Button( c, SWT.PUSH );
//    m_button.setText( "..." );
//    m_button.addSelectionListener( this );
//
    return c;
  }

//  /**
//   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
//   */
//  public void widgetSelected( final SelectionEvent e )
//  {
//    // show FileDialog!
//    final FileDialog dialog = new FileDialog( m_button.getShell(), SWT.OPEN );
//    dialog.setFilterExtensions( new String[]
//    { "*.shp" } );
//    dialog.setFilterNames( new String[]
//    { "ESRI Shape (*.shp)" } );
//
//    final String fileName = dialog.open();
//    if( fileName != null )
//    {
//      m_text.setText( fileName );
//      m_source = fileName;
//    }
//  }
//
//  /**
//   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
//   */
//  public void widgetDefaultSelected( final SelectionEvent e )
//  {
//  // nix tun
//  }

}