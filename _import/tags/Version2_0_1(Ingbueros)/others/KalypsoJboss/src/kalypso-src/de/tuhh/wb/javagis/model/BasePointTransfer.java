package de.tuhh.wb.javagis.model;

import java.io.Serializable;

public class BasePointTransfer implements Serializable
{
    private double x;
    private double y;
    public BasePointTransfer(double x,double y)
    {
	this.x=x;
	this.y=y;
    }
    public double getX()
    {
	return x;
    }
    public double getY()
    {
	return y;
    }
}

