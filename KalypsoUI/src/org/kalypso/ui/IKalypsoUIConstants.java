package org.kalypso.ui;

/**
 * Constants for the Kalypso UI.
 * 
 * Not intended to be implemented nor extended.
 * 
 * @author schlienger
 */
public interface IKalypsoUIConstants
{
  /** Observation Diagram View identifier (value <code>org.kalypso.ogc.sensor.view.DiagramViewPart</code>) */
  public final static String ID_OBSDIAGRAM_VIEW = "org.kalypso.ogc.sensor.view.DiagramViewPart"; //$NON-NLS-1$

  /** Observation Table View identifier (value <code>org.kalypso.ogc.sensor.view.TableViewPart</code>) */
  public final static String ID_OBSTABLE_VIEW = "org.kalypso.ogc.sensor.view.TableViewPart"; //$NON-NLS-1$
  
  /** Repository View identifier (value <code>org.kalypso.ui.repository.view.RepositoryExplorerPart</code>) */
  public final static String ID_REPOSITORY_VIEW = "org.kalypso.ui.repository.view.RepositoryExplorerPart";

  public static final String MODELER_PERSPECTIVE = "org.kalypso.ui.perspectives.ModelerPerspectiveFactory"; //$NON-NLS-1$
}