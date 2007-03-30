/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.featureview.views;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.xml.sax.InputSource;

/**
 * @author kuch
 */
public class FeatureCompositeGFTWrapper
{
  private IFeatureChangeListener m_ifFtrChLstner;

  private FeatureComposite m_compFeature;

  private final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  public void show( final IFile file, final Feature feature, final Composite composite, final FormToolkit toolkit ) throws UnsupportedEncodingException, CoreException, JAXBException
  {
    show( file, new CommandableWorkspace( feature.getWorkspace() ), feature, composite, toolkit );
  }

  public void show( final IFile file, final CommandableWorkspace workspace, final Feature feature, final Composite composite, final FormToolkit toolkit ) throws UnsupportedEncodingException, CoreException, JAXBException
  {
    /* Create a new FeatureComposite. */
    final FeatureviewHelper fvFactory = new FeatureviewHelper();
    fvFactory.setShouldAddValidator( true );
    fvFactory.setShouldShowButton( false );
    fvFactory.setShowTables( false );

    final CachedFeatureviewFactory cfvFactory = new CachedFeatureviewFactory( fvFactory );

    final Reader r;

    r = new InputStreamReader( file.getContents(), ((IEncodedStorage) file).getCharset() );

    final InputSource is = new InputSource( r );
    final Unmarshaller unmarshaller = FeatureCompositeGFTWrapper.JC.createUnmarshaller();

    final Featuretemplate m_template = (Featuretemplate) unmarshaller.unmarshal( is );
    final List<FeatureviewType> view = m_template.getView();

    for( final FeatureviewType featureviewType : view )
    {
      cfvFactory.addView( featureviewType );
    }

    m_compFeature = new FeatureComposite( feature, KalypsoCorePlugin.getDefault().getSelectionManager(), cfvFactory );
    m_compFeature.setFormToolkit( toolkit );
    m_compFeature.setFeature( feature );
    m_compFeature.setShowOk( false );
    m_compFeature.createControl( composite, SWT.NONE );

    removeListener(workspace);
    addListener( workspace );
  }

  private void removeListener( CommandableWorkspace workspace )
  {
    if (m_compFeature != null && m_ifFtrChLstner != null)
    {
      m_compFeature.removeChangeListener( m_ifFtrChLstner );
      m_ifFtrChLstner = null;
    }
    
  }

  private void addListener( final CommandableWorkspace workspace )
  {

    /*
     * If the workspace is not initialized, leave the function. This makes sure, that the function createGML() or
     * loadGML() is at least called once.
     */
    if( workspace == null )
    {
      throw new IllegalStateException( "Workspace not initialized." );
    }

    /* If a FeatureComposite exists, then the listeners can be attached. */
    if( m_compFeature == null )
    {
      throw new IllegalStateException( "There was no FeatureComposite." );
    }

    /*
     * Are there already listeners created?
     */
    if( (m_ifFtrChLstner != null) )
    {
      /* There are already listeners specified. Remove them first. */
      throw new IllegalStateException( "There are already listener attached, remove them first." );
    }

    /* ModellListener erzeugen. */
    final ModellEventListener listener = new ModellEventListener()
    {
      public void onModellChange( final ModellEvent modellEvent )
      {
        if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
        {
          final Control control = getFeatureComposite().getControl();
          if( (control != null) && !control.isDisposed() )
          {
            control.getDisplay().asyncExec( new Runnable()
            {
              public void run( )
              {
                if( !control.isDisposed() )
                {
                  getFeatureComposite().updateControl();
                }
              }
            } );
          }
        }
      }
    };
    workspace.addModellListener( listener );

    m_ifFtrChLstner = new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        try
        {
          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { change } );
          workspace.postCommand( command );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }

      }

      public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
      {
      }
    };
    m_compFeature.addChangeListener( m_ifFtrChLstner );
  }

  public FeatureComposite getFeatureComposite( )
  {
    return m_compFeature;
  }

  /**
   * GFT Files often points to an special Feature - with this function, you can replace an PLACEHOLDER with a feature
   * id.
   * 
   * @param project
   *          your active eclipse project
   * @param templateFilePath
   *          project relative file name
   * @param gftFile
   *          filePointer to new gft-File which will be created
   * @param gmlId
   *          the id which will be replaced
   */
  public void createGftFile( final IProject project, final String templateFilePath, final File gftFile, final String gmlId ) throws IOException, CoreException
  {
    final HashMap<String, String> replacement = new HashMap<String, String>();
    replacement.put( "%PLACEHOLDER%", gmlId );

    createGftFile( project, templateFilePath, gftFile, replacement );
  }

  /**
   * dto. - in this function you can replace different kind of keys and values (hashmap)
   * 
   * @param project
   *          your active eclipse project
   * @param templateFilePath
   *          project relative file name
   * @param gftFile
   *          filePointer to new gft-File which will be created
   * @param hReplacements
   *          index[key, value] - keys will be replaced with their assigned values
   */
  public void createGftFile( final IProject project, final String templateFilePath, final File gftFile, final Map<String, String> hReplacements ) throws IOException, CoreException
  {
    if( (project == null) || (templateFilePath == null) || (gftFile == null) || (hReplacements == null) )
    {
      return;
    }

    final IFile iFile = project.getFile( templateFilePath );
    final String sTempLocation = iFile.getLocation().toOSString();

    final FileReader input = new FileReader( sTempLocation );

    if( input == null )
    {
      return;
    }

    String content = "";
    int c;

    while( (c = input.read()) != -1 )
    {
      // $ANALYSIS-IGNORE,codereview.java.rules.casting.RuleCastingPrimitives
      content += (char) c;

    }

    input.close();

    final Set<Entry<String, String>> entrySet = hReplacements.entrySet();

    for( final Entry<String, String> entry : entrySet )
    {
      content = content.replaceAll( entry.getKey(), entry.getValue() );
    }

    final FileWriter writer = new FileWriter( gftFile );
    writer.write( content );
    writer.close();

    project.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
  }
}
