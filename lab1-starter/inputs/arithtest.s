        # Basic arithmetic instructions
        # This is a hodgepodge of arithmetic instructions to test
        # your basic functionality.
        # No overflow exceptions should occur
	.text
main:   
        addiu   $s2, $zero, 1024 # $s2 = 1024
        addu    $3, $2, $2      # $3 = $2 + $2
        or      $4, $3, $2     # $4 = $3 | $2
        add     $5, $zero, 1234 # $5 = 1234
        sll     $6, $5, 16     # $6 = $5 << 16
        addiu   $7, $6, 9999   # $7 = $6 + 9999
        subu    $8, $7, $2    # $8 = $7 - $2
        xor     $9, $4, $3   # $9 = $4 ^ $3
        xori    $10, $2, 255 # $10 = $2 ^ 255
        srl     $11, $6, 5  # $11 = $6 >> 5
        sra     $12, $6, 4 # $12 = $6 >> 4
        and     $13, $11, $5 # $13 = $11 & $5
        andi    $14, $4, 100 # $14 = $4 & 100
        sub     $15, $zero, $10 # $15 = 0 - $10
        lui     $17, 100 # $17 = 100 << 16
        addiu   $v0, $zero, 0xa # syscall 10
        syscall
        
        
                        
