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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.wind;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Composite command used to change the wind data model command. This composite takes the responsibility to notifies the
 * commandable workspace about the changes introduced by its sub command
 * 
 * 
 * @author ig
 * 
 */
public class ChangeWindDataSystemCommand implements ICommand
{
  public static final String DEFAULT_DESCRIPTION = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeWindModelCommand.0" ); //$NON-NLS-1$

  private final CommandableWorkspace m_commandableWorkspace;

  private final List<IFeatureChangeCommand> m_commands = new ArrayList<IFeatureChangeCommand>();

  private final List<IFile> m_files = new ArrayList<IFile>();

  private int m_intEventType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD;

  private boolean isUndoable = true;

  private final IWindModel m_windModel;

  private final String m_description;

  public ChangeWindDataSystemCommand( final CommandableWorkspace commandableWorkspace, final IWindModel pWindModel )
  {
    this( commandableWorkspace, pWindModel, DEFAULT_DESCRIPTION );
  }

  public ChangeWindDataSystemCommand( final CommandableWorkspace commandableWorkspace, final IWindModel pWindModel, final String description )
  {
    Assert.throwIAEOnNullParam( pWindModel, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.1" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( description, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.2" ) ); //$NON-NLS-1$
    m_commandableWorkspace = commandableWorkspace;
    m_windModel = pWindModel;
    m_description = description;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return isUndoable;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @SuppressWarnings("deprecation")
  public void process( ) throws Exception
  {
    final List<Feature> changedFeatures = new ArrayList<Feature>();
    for( final IFeatureChangeCommand command : m_commands )
    {
      try
      {
        command.process();
        final IFeatureWrapper2[] changedFeatures2 = command.getChangedFeature();
        if( changedFeatures2 != null )
        {
          for( final IFeatureWrapper2 changedFeature : changedFeatures2 )
          {
            if( changedFeature != null )
            {
              final Feature wrappedFeature = changedFeature.getFeature();
              if( wrappedFeature != null )
              {
                changedFeatures.add( wrappedFeature );
                wrappedFeature.invalidEnvelope();
              }
            }
          }
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    fireStructureChange( changedFeatures );
  }

  private final void fireStructureChange( final List<Feature> changedFeatures )
  {
    final Feature[] changedFeaturesArray = new Feature[changedFeatures.size()];
    changedFeatures.toArray( changedFeaturesArray );
    m_commandableWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_commandableWorkspace, m_windModel.getFeature(), changedFeaturesArray, m_intEventType ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    for( final IFeatureChangeCommand command : m_commands )
    {
      try
      {
        command.redo();
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

    // reverse order is taken because of eventual dependencies
    IFeatureChangeCommand command;
    for( int index = m_commands.size() - 1; index >= 0; index-- )
    {
      command = m_commands.get( index );
      try
      {
        command.undo();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

  }

  public void addCommand( final IFeatureChangeCommand command )
  {
    Assert.throwIAEOnNullParam( command, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeWindModelCommand.3" ) ); //$NON-NLS-1$
    m_commands.add( command );
    if( command instanceof DeleteWindDataSystem )
    {
      for( final IWindDataProvider lWindDataWrapper : ((DeleteWindDataSystem) command).getWindDataModelSystem().getWindDataModels() )
      {
        lWindDataWrapper.getDataAsGrid().dispose();
        IFile lIFile;
        try
        {
          File lFile = new File( lWindDataWrapper.getDataFileURL().toURI() );
          IPath path = new Path( lFile.getAbsolutePath() );
          lIFile = ResourcesPlugin.getWorkspace().getRoot().getFile( path );
//          lFile = new IFile( lWindDataWrapper.getDataFileURL().toURI() );
        }
        catch( URISyntaxException e )
        {
          File lFile = new File( lWindDataWrapper.getDataFileURL().getPath() );
          IPath path = new Path( lFile.getAbsolutePath() );
          lIFile = ResourcesPlugin.getWorkspace().getRoot().getFile( path );
        }
        m_files.add( lIFile );
      }
      m_intEventType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;
    }

    isUndoable = isUndoable && command.isUndoable();

  }

  public IStatus deleteFiles( )
  {
    final MultiStatus status = new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), 1, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeWindSystemCommand.4" ), null ); //$NON-NLS-1$
    for( final IFile lFile : m_files )
    {
      try
      {
         lFile.delete( false, new NullProgressMonitor() );
//        lFile.deleteOnExit();
      }
      catch( final Exception e )
      {
      }
    }

    return status;
  }

}
