/*
 * OpenGIS� Coordinate Transformation Services Implementation Specification
 * Copyright (2001) OpenGIS consortium
 *
 * THIS COPYRIGHT NOTICE IS A TEMPORARY PATCH.   Version 1.00 of official
 * OpenGIS's interface files doesn't contain a copyright notice yet. This
 * file is a slightly modified version of official OpenGIS's interface.
 * Changes have been done in order to fix RMI problems and are documented
 * on the SEAGIS web site (seagis.sourceforge.net). THIS FILE WILL LIKELY
 * BE REPLACED BY NEXT VERSION OF OPENGIS SPECIFICATIONS.
 */
package org.opengis.cs;

// JDK's classes
import java.rmi.RemoteException;


/**
 * Definition of linear units.
 *
 * @version 1.01
 * @since   1.00
 * @author Martin Daly
 */
public interface CS_LinearUnit extends CS_Unit
{
    /**
     * Returns the number of meters per LinearUnit.
     *
     * @throws RemoteException if a remote method call failed.
     */
    double getMetersPerUnit() throws RemoteException;
}
