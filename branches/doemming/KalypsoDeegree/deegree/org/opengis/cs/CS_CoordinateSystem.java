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

// OpenGIS dependencies
import org.opengis.pt.PT_Envelope;

// JDK's classes
import java.rmi.RemoteException;


/**
 * Base interface for all coordinate systems.
 * A coordinate system is a mathematical space, where the elements of
 * the space are called positions.  Each position is described by a list
 * of numbers.  The length of the list corresponds to the dimension of
 * the coordinate system.  So in a 2D coordinate system each position is
 * described by a list containing 2 numbers.
 * <br><br>
 * However, in a coordinate system, not all lists of numbers correspond
 * to a position - some lists may be outside the domain of the coordinate
 * system.  For example, in a 2D Lat/Lon coordinate system, the list (91,91)
 * does not correspond to a position.
 * <br><br>
 * Some coordinate systems also have a mapping from the mathematical space
 * into locations in the real world.  So in a Lat/Lon coordinate system, the
 * mathematical position (lat, long) corresponds to a location on the surface
 * of the Earth.  This mapping from the mathematical space into real-world
 * locations is called a Datum.
 *
 * @version 1.01
 * @since   1.00
 * @author Martin Daly
 */
public interface CS_CoordinateSystem extends CS_Info
{
    /**
     * Dimension of the coordinate system.
     *
     * @throws RemoteException if a remote method call failed.
     */
    int getDimension() throws RemoteException;

    /**
     * Gets axis details for dimension within coordinate system.
     * Each dimension in the coordinate system has a corresponding axis.
     *
     * @param dimension Zero based index of axis.
     * @throws RemoteException if a remote method call failed.
     */
    CS_AxisInfo getAxis(int dimension) throws RemoteException;

    /**
     * Gets units for dimension within coordinate system.
     * Each dimension in the coordinate system has corresponding units.
     *
     * @param dimension Zero based index of axis.
     * @throws RemoteException if a remote method call failed.
     */
    CS_Unit getUnits(int dimension) throws RemoteException;

    /**
     * Gets default envelope of coordinate system.
     * Coordinate systems which are bounded should return the minimum bounding
     * box of their domain.  Unbounded coordinate systems should return a box
     * which is as large as is likely to be used.  For example, a (lon,lat)
     * geographic coordinate system in degrees should return a box from
     * (-180,-90) to (180,90), and a geocentric coordinate system could return
     * a box from (-r,-r,-r) to (+r,+r,+r) where r is the approximate radius
     * of the Earth.
     *
     * @throws RemoteException if a remote method call failed.
     */
    PT_Envelope getDefaultEnvelope() throws RemoteException;
}
