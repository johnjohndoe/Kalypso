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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Undoable command that changes the name of of an {@link IFeatureWrapper2}
 * 
 * @author Patrice Congo
 * 
 */
public class ChangeIFeatureWrapper2NameCmd implements IDiscrModel1d2dChangeCommand
{

  private String oldName;

  private final IFeatureWrapper2 featureToRename;

  private final String newName;

  /**
   * Sets to true to indicate process done
   */
  private boolean changed = false;

  /**
   * Creates a Calculation unit of the given q-name
   * 
   * @param cuFeatureQName
   *          the q-name of the calculation unit to create
   * @param model1d2d
   *          the model that should hold the new calculation unit
   * @param name
   *          a name for the calculation unit if one has to be set or null
   * @param description
   *          text describing the calculation unit or null
   * @throws IllegalArgumentException
   *           if cuFeatureQName or model1d2d is null
   */
  @SuppressWarnings("hiding")
  public ChangeIFeatureWrapper2NameCmd( IFeatureWrapper2 featureToRename, String newName )
  {
    Assert.throwIAEOnNullParam( featureToRename, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd.0" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( newName, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd.1" ) ); //$NON-NLS-1$
    this.featureToRename = featureToRename;
    this.newName = newName;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( changed )
    {
      return new IFeatureWrapper2[] { featureToRename };
    }
    else
    {
      return new IFeatureWrapper2[] {};
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd.2" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    try
    {
      changed = true;
      oldName = featureToRename.getName();
      featureToRename.setName( newName );
      fireProcessChanges();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  /**
   * 
   * @param calculationUnit
   *          the added or removed calculation unit
   * @param added
   *          true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( )
  {
    final int changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;

    GMLWorkspace workspace = featureToRename.getFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, featureToRename.getFeature().getParent(), new Feature[] { featureToRename.getFeature() }, changedType );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( !changed )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    featureToRename.setDescription( oldName );
    fireProcessChanges();
  }

}
