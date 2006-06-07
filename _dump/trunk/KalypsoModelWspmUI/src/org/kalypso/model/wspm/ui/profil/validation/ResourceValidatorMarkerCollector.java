package org.kalypso.model.wspm.ui.profil.validation;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.ide.IDE;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


final class ResourceValidatorMarkerCollector implements IValidatorMarkerCollector
{
  private final IResource m_resource;

  private final static String[] USED_ATTRIBUTES = new String[] { IMarker.MESSAGE, IMarker.LOCATION, IMarker.SEVERITY, IMarker.TRANSIENT, IDE.EDITOR_ID_ATTR, IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPROPERTY };

  private final String m_editorID;

  public ResourceValidatorMarkerCollector( final IResource resource, final String editorID )
  {
    m_resource = resource;
    m_editorID = editorID;
  }

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method, so changes in the
   * implementation (e.g. the type of the marker) are reflekted on all rules.
   * 
   * @throws CoreException
   */
  public void createProfilMarker( boolean isSevere, final String message, final String location, final int pointPos, final String pointProperty ) throws CoreException
  {
    final IMarker marker = m_resource.createMarker( KalypsoModelWspmUIPlugin.MARKER_ID );

    final Object[] values = new Object[] { message, location, isSevere ? IMarker.SEVERITY_ERROR : IMarker.SEVERITY_WARNING, true, m_editorID, pointPos, pointProperty };

    marker.setAttributes( USED_ATTRIBUTES, values );
  }

  public void reset( ) throws CoreException
  {
    m_resource.deleteMarkers( KalypsoModelWspmUIPlugin.MARKER_ID, true, IResource.DEPTH_ZERO );
  }
}