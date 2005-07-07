package com.bce.eind.core.profil.validator;

import org.eclipse.core.runtime.CoreException;

public interface IValidatorMarkerCollector
{
  public static final String MARKER_ATTRIBUTE_POINTPOS = "profile.marker.attribute.pointpos";

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method,
   * so changes in the implementation (e.g. the type of the marker) are reflekted on all rules.
   * 
   * @throws CoreException
   */
  public void createProfilMarker( final boolean isSevere, final String message,
      final String location, final int pointPos ) throws CoreException;
}
